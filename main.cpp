/*
  Supply Drop
  Copyright (C) 2023 Bernhard Schelling

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.
*/

#include <ZL_Application.h>
#include <ZL_Display.h>
#include <ZL_Surface.h>
#include <ZL_Signal.h>
#include <ZL_Audio.h>
#include <ZL_Font.h>
#include <ZL_Input.h>
#include <ZL_Particles.h>
#include <ZL_SynthImc.h>
#include <../Opt/chipmunk/chipmunk.cpp>

enum { ROPE_KNOTS = 10, MAX_TURRETS = 6, MAX_LEVELS = 5 };
static cpSpace *space;
static cpBody *floorbody, *helibody, *boxbody, *knotbodies[ROPE_KNOTS], *turretbodies[MAX_TURRETS];
static cpShape *padshapes[3+MAX_TURRETS], *turretbaseshapes[MAX_TURRETS];
static cpVect turretpos[MAX_TURRETS], boxstartpos, goalpos, ggravity = cpv(0.0f, -162.f);
static ticks_t levelstart, lastbullet[MAX_TURRETS], attachtime, detachtime, dietime, wintime, endtitletime;
static std::vector<cpBody*> missiles;
static ZL_Surface srfHeli, srfBox, srfCement, srfMissile, srfArrow;
static ZL_Font fntMain, fntBig;
static ZL_ParticleEffect particleFire;
static float heightmap[256], waterlevel;
static int curlevel, wincount, losecount;
extern ZL_SynthImcTrack imcMusic, imcHeli;
extern TImcSongData imcDataIMCATTACH, imcDataIMCHIT, imcDataIMCBOOM, imcDataIMCWIN;
static ZL_Sound sndAttach, sndHit, sndBoom, sndWin;
static const ZL_Color watercolor = ZLRGBA(.1, .1, .6, .95f);
static const ZL_Color skycolors[MAX_LEVELS][2] = {
	{ ZLRGB(0,0,.3),    ZLRGBX(0x99D9EA) }, // blue
	{ ZLRGBX(0x013378), ZLRGBX(0xBEE3F5) }, // light blue
	{ ZLRGBX(0x080D13), ZLRGBX(0xB2C9DB) }, // gray
	{ ZLRGBX(0x6B2967), ZLRGBX(0xA73B3B) }, // purple-red 
	{ ZLRGBX(0x3D1C27), ZLRGBX(0xD96A27) }, // red-orange
};
static const ZL_Color landcolors[MAX_LEVELS] = {
	ZLRGBX(0x0DC730),
	ZLRGBX(0x2FDA26),
	ZLRGBX(0x3FF96F),
	ZLRGBX(0x339A22),
	ZLRGBX(0x198165),
};
static const int turretperlevel[MAX_LEVELS] = { 0, 1, 2, 4, 6, };

enum CollisionTypes
{
	COLLISION_LAND,
	COLLISION_HELI,
	COLLISION_ROPE,
	COLLISION_BOX,
	COLLISION_MISSILE,
};

static void ResetBox()
{
	sndBoom.SetVolume(1.0f).Play();
	particleFire.Spawn(20, cpBodyGetPosition(boxbody));
	cpBodySetPosition(boxbody, boxstartpos);
	cpBodySetVelocity(boxbody, cpvzero);
	cpBodySetAngle(boxbody, 0);
	cpBodySetAngularVelocity(boxbody, 0);
	losecount = 0;
}

static void PostStepRemoveBody(cpSpace *space, cpBody* body, void* data)
{
	CP_BODY_FOREACH_SHAPE(body, shape) cpSpaceRemoveShape(space, shape);
	CP_BODY_FOREACH_CONSTRAINT(body, constraint) cpSpaceRemoveConstraint(space, constraint);
	cpSpaceRemoveBody(space, body);
}

static void PostStepResetBox(cpSpace *space, void *key, void *data)
{
	if (boxbody->constraintList) cpSpaceRemoveConstraint(space, boxbody->constraintList);
	ResetBox();
}

static void NoGravityVelocityFunc(cpBody *body, cpVect gravity, cpFloat damping, cpFloat dt) {}

static cpBool CollisionIgnore(cpArbiter *arb, cpSpace *space, cpDataPointer userData) { return cpFalse; }

static cpBool CollisionHeliOrBox2LandOrHeli(cpArbiter *arb, cpSpace *space, cpDataPointer userData)
{
	CP_ARBITER_GET_BODIES(arb, bBody, bLand);
	float speed = cpvlength(cpBodyGetVelocity(bBody));
	if (speed > 40)
	{
		sndHit.SetVolume(ZL_Math::Clamp01((speed - 40.0f)/100.0f));
		sndHit.Play();
	}
	if (speed > 320)
	{
		if (bBody == helibody)
		{
			sndBoom.SetVolume(1.0f).Play();
			particleFire.Spawn(50, cpBodyGetPosition(bBody));
			dietime = ZLTICKS;
			imcHeli.Stop();
		}
		if (bBody == boxbody)
		{
			cpSpaceAddPostStepCallback(space, PostStepResetBox, NULL, NULL);
		}
	}
	return cpTrue;
}

static cpBool CollisionRope2Box(cpArbiter *arb, cpSpace *space, cpDataPointer userData)
{
	return ((arb->body_a != knotbodies[COUNT_OF(knotbodies)-1] && arb->body_b != knotbodies[COUNT_OF(knotbodies)-1]) ? cpTrue : cpFalse);
}

static cpBool CollisionMissile2HeliOrBox(cpArbiter *arb, cpSpace *space, cpDataPointer userData)
{
	CP_ARBITER_GET_BODIES(arb, bMissile, bHeliOrBox);
	cpVect vel = cpBodyGetVelocity(bMissile), pos = cpBodyGetPosition(bMissile);
	cpSpaceAddPostStepCallback(space, (cpPostStepFunc)PostStepRemoveBody, bMissile, NULL);
	size_t it = std::find(missiles.begin(), missiles.end(), bMissile) - missiles.begin();
	if (it != missiles.size()) // handle hitting 2 things in the same tick
	{
		particleFire.Spawn(50, pos);
		sndBoom.SetVolume(0.8f).Play();
		missiles[it] = missiles.back();
		missiles.pop_back();
	}
	cpBodyApplyImpulseAtLocalPoint(bHeliOrBox, cpvmult(vel, cpBodyGetMass(bHeliOrBox)*0.5f), cpvzero);
	return cpFalse;
}

static void DrawTextBordered(const ZL_TextBuffer& buf, const ZL_Vector& p, scalar scale = 1, const ZL_Color& colfill = ZLWHITE, const ZL_Color& colborder = ZLBLACK, int border = 2, ZL_Origin::Type origin = ZL_Origin::Center)
{
	for (int i = 0; i < 9; i++) if (i != 4) buf.Draw(p.x+(border*((i%3)-1)), p.y+(border*((i/3)-1)), scale, scale, colborder, origin);
	buf.Draw(p.x, p.y, scale, scale, colfill, origin);
}

static float ls(float x)
{
	static unsigned char perm[512];
	if (!perm[0]) { ZL_SeededRand r(123); for (unsigned char& i : perm) i = (unsigned char)(r.UInt() & 0xFF); }
	struct Local
	{
		static float grad1(int hash, float x)
		{
			int h = hash & 15;
			float grad = 1.0f + (h & 7);
			if (h&8) grad = -grad;
			return ( grad * x );
		}
		static float snoise1(float x)
		{
			int i0 = (((int)(x)<=(x)) ? ((int)x) : (((int)x)-1)), i1 = i0 + 1;
			float x0 = x - i0, x1 = x0 - 1.0f, t0 = ZL_Math::Square(1.0f - x0*x0), t1 = ZL_Math::Square(1.0f - x1*x1);
			return 0.25f * ((t0*t0 * grad1(perm[i0 & 0xff], x0)) + (t1*t1 * grad1(perm[i1 & 0xff], x1)));
		}
	};
	return Local::snoise1(x) + Local::snoise1(10+x*4)*.1f;
}

static float lsx(int i)
{
	return i*20.0f;
}

static int FindLandingSpot(int from = 0, int addmin = 1)
{
	if (from < 0) return -1;
	for (int i = from + addmin; i < COUNT_OF(heightmap)-2; i++)
	{
		if (heightmap[i] > waterlevel+0.1f && heightmap[i+1] > waterlevel+0.1f && heightmap[i+2] > waterlevel+0.1f && heightmap[i+3] > waterlevel+0.1f)
			return i;
	}
	return -1;
}

static void Load()
{
	srfHeli = ZL_Surface("Data/heli.png").SetScale(0.5f).SetOrigin(ZL_Origin::Custom(0.63f, 0.5f));
	srfBox = ZL_Surface("Data/box.png");
	srfCement = ZL_Surface("Data/cement.png");
	srfMissile = ZL_Surface("Data/missile.png").SetScale(0.5f).SetOrigin(ZL_Origin::Custom(0.63f, 0.5f));
	srfArrow = ZL_Surface("Data/arrow.png").SetScale(0.5f).SetOrigin(ZL_Origin::Center);
	fntMain = ZL_Font("Data/vipond_chubby.ttf.zip", 32);
	fntBig = ZL_Font("Data/matchbox.ttf.zip", 80);
	sndAttach = ZL_SynthImcTrack::LoadAsSample(&imcDataIMCATTACH);
	sndHit = ZL_SynthImcTrack::LoadAsSample(&imcDataIMCHIT);
	sndBoom = ZL_SynthImcTrack::LoadAsSample(&imcDataIMCBOOM);
	sndWin = ZL_SynthImcTrack::LoadAsSample(&imcDataIMCWIN);
	imcMusic.Play();

	particleFire = ZL_ParticleEffect(500, 200);
	particleFire.AddParticleImage(ZL_Surface("Data/particle.png").SetColor(ZLRGB(1,.8,.1)), 200);
	particleFire.AddBehavior(new ZL_ParticleBehavior_LinearMove(50, 10));
	particleFire.AddBehavior(new ZL_ParticleBehavior_LinearImageProperties(1, 0, 1, 3));
}

static void Init(int level = 0)
{
	int oldlevel = curlevel;
	curlevel = (level ? level : 1);
	memset(padshapes, 0, sizeof(padshapes));
	memset(turretbodies, 0, sizeof(turretbodies));
	memset(turretbaseshapes, 0, sizeof(turretbaseshapes));
	memset(lastbullet, 0, sizeof(lastbullet));
	attachtime = detachtime = dietime = wintime = 0;
	wincount = losecount = 0;
	missiles.clear();

	static unsigned int rand_seed;
	if (oldlevel != curlevel) rand_seed = ZL_Rand::UInt();
	ZL_SeededRand levelrand(rand_seed);

	newlandscape:
	float total = 0;
	float lsstart = levelrand.Range(0, 1000);
	while (ls(lsstart) > -.25f)
		lsstart += 0.1f;

	for (int i = 0; i != COUNT_OF(heightmap); i++)
	{
		heightmap[i] = ls(lsstart+i*0.05f) * 250 + 250;
		total += heightmap[i];
	}

	int turretcount = turretperlevel[curlevel-1];
	int spots[COUNT_OF(padshapes)], helispot = 0, boxspot = 1, goalspot = boxspot + 1 + turretcount;
	memset(spots, 0, sizeof(spots));

	waterlevel = total / COUNT_OF(heightmap);
	if (heightmap[COUNT_OF(heightmap)-1] > waterlevel) goto newlandscape;

	spots[helispot] = FindLandingSpot();
	spots[boxspot] = FindLandingSpot(spots[0], 5);
	for (int i = boxspot + 1; i != goalspot; i++)
	{
		spots[i] = FindLandingSpot(spots[i-1], 30);
	}
	spots[goalspot] = FindLandingSpot(spots[goalspot-1], 30);
	if (spots[goalspot] == -1) goto newlandscape;

	if (space) cpSpaceFree(space);
	space = cpSpaceNew();
	cpSpaceSetGravity(space, ggravity);
	cpSpaceAddCollisionHandler(space, COLLISION_HELI, COLLISION_LAND)->beginFunc = CollisionHeliOrBox2LandOrHeli;
	cpSpaceAddCollisionHandler(space, COLLISION_BOX, COLLISION_LAND)->beginFunc = CollisionHeliOrBox2LandOrHeli;
	cpSpaceAddCollisionHandler(space, COLLISION_BOX, COLLISION_HELI)->beginFunc = CollisionHeliOrBox2LandOrHeli;
	cpSpaceAddCollisionHandler(space, COLLISION_ROPE, COLLISION_ROPE)->beginFunc = CollisionIgnore;
	cpSpaceAddCollisionHandler(space, COLLISION_ROPE, COLLISION_HELI)->beginFunc = CollisionIgnore;
	cpSpaceAddCollisionHandler(space, COLLISION_ROPE, COLLISION_BOX)->beginFunc = CollisionRope2Box;
	cpSpaceAddCollisionHandler(space, COLLISION_MISSILE, COLLISION_HELI)->beginFunc = CollisionMissile2HeliOrBox;
	cpSpaceAddCollisionHandler(space, COLLISION_MISSILE, COLLISION_BOX)->beginFunc = CollisionMissile2HeliOrBox;

	floorbody = cpSpaceAddBody(space, cpBodyNew(999999999.f, INFINITY));
	cpBodySetVelocityUpdateFunc(floorbody, NoGravityVelocityFunc);

	for (int x = 0; x != COUNT_OF(heightmap)-1; x++)
	{
		float x1 = lsx(x)-.01f, x2 = lsx(x+1)+.01f, y1 = heightmap[x], y2 = heightmap[x+1];
		cpVect verts[] = { {x1, y1}, {x2, y2}, {x2, 0}, {x1, 0}, };
		cpShape *floorshape = cpSpaceAddShape(space, cpPolyShapeNew(floorbody, 4, verts, cpTransformIdentity, 0));
		cpShapeSetFriction(floorshape, .95f);
	}

	float padys[COUNT_OF(padshapes)];
	for (int i = 0; i != COUNT_OF(padshapes); i++)
	{
		int x = spots[i], ix1 = x + (i > boxspot && i < goalspot ? 1 : 0), ix2 = x + 3;
		if (!x) break;;
		float x1 = lsx(ix1)+3.f, x2 = lsx(ix2)-3.f, y = ZL_Math::Max(ZL_Math::Max(ZL_Math::Max(heightmap[x], heightmap[x+1]), heightmap[x+2]), heightmap[x+3]) + 10;
		cpVect verts[] = { {x1, y}, {x2, y}, {x2, 0}, {x1, 0}, };
		cpShape *floorshape = cpSpaceAddShape(space, cpPolyShapeNew(floorbody, 4, verts, cpTransformIdentity, 0));
		cpShapeSetFriction(floorshape, .95f);
		padys[i] = y;
		padshapes[i] = floorshape;
	}

	for (int i = boxspot + 1; i != goalspot; i++)
	{
		cpVect ctr = cpv(lsx(spots[i]+2), padys[i] + 18);
		cpVect verts[] = { {ctr.x-10, ctr.y+18}, {ctr.x+10, ctr.y+18}, {ctr.x+10, ctr.y-18}, {ctr.x-10, ctr.y-18}, };
		cpShape *floorshape = cpSpaceAddShape(space, cpPolyShapeNew(floorbody, 4, verts, cpTransformIdentity, 0));
		cpShapeSetFriction(floorshape, .95f);
		turretbaseshapes[i - boxspot - 1] = floorshape;
		turretpos[i - boxspot - 1] = cpvadd(ctr, cpv(0, 15));
	}

	const float helimass = 50.0f;
	const float knotmass = .1f;
	const float boxmass = 1.0f;

	cpVect helipos = cpv(ZL_Math::Lerp(lsx(spots[helispot]+1), lsx(spots[helispot]+2), 0.5f), padys[helispot]+14);
	boxstartpos = cpv(ZL_Math::Lerp(lsx(spots[boxspot]+1), lsx(spots[boxspot]+2), 0.5f), padys[boxspot]+15);
	goalpos = cpv(ZL_Math::Lerp(lsx(spots[goalspot]+1), lsx(spots[goalspot]+2), 0.5f), padys[goalspot]+5);

	helibody = cpSpaceAddBody(space, cpBodyNew(helimass, cpMomentForCircle(helimass, 0, 25, cpvzero)));
	cpBodySetPosition(helibody, helipos);
	cpShape *helishape1 = cpSpaceAddShape(space, cpBoxShapeNew2(helibody, cpBBNew(-15, -14, 20, 15), 0));
	cpShape *helishape2 = cpSpaceAddShape(space, cpBoxShapeNew2(helibody, cpBBNew(-50, -5, -15, 5), 0));
	cpShapeSetCollisionType(helishape1, COLLISION_HELI);
	cpShapeSetCollisionType(helishape2, COLLISION_HELI);

	for (int i = 0; i != COUNT_OF(knotbodies); i++)
	{
		knotbodies[i] = cpSpaceAddBody(space, cpBodyNew(knotmass, cpMomentForCircle(knotmass, 0, 3, cpvzero)));
		cpShape* knotshape = cpSpaceAddShape(space, cpCircleShapeNew(knotbodies[i], 3, cpvzero));
		cpBodySetPosition(knotbodies[i], cpv(helipos.x, helipos.y - 15));
		cpShapeSetCollisionType(knotshape, COLLISION_ROPE);

		cpConstraint* joint = cpSlideJointNew(knotbodies[i], i ? knotbodies[i-1] : helibody,  cpvzero, cpv(0.0f, i ? 0.0f : -15.0f), 0.0f, 15.0f);
		cpSpaceAddConstraint(space, joint);
	}

	boxbody = cpSpaceAddBody(space, cpBodyNew(boxmass, cpMomentForCircle(boxmass, 0, 25, cpvzero)));
	cpBodySetPosition(boxbody, boxstartpos);
	cpShape *boxshape = cpSpaceAddShape(space, cpBoxShapeNew2(boxbody, cpBBNew(-15, -15, 15, 15), 0));
	cpShapeSetFriction(boxshape, .95f);
	cpShapeSetCollisionType(boxshape, COLLISION_BOX);

	if (!level) { endtitletime = levelstart = 0; imcHeli.Stop(); }
	else { endtitletime = 1; levelstart = ZLTICKS; imcHeli.Play(); }
}

static void Frame()
{
	if (ZL_Input::Down(ZLK_ESCAPE))
	{
		if (!endtitletime) ZL_Application::Quit();
		Init();
	}
	#ifdef ZILLALOG //DEBUG DRAW
	if (ZL_Input::Down(ZLK_F4)) Init(ZL_Math::Max(curlevel-1, 1));
	if (ZL_Input::Down(ZLK_F5)) Init(curlevel);
	if (ZL_Input::Down(ZLK_F6)) Init(ZL_Math::Min(curlevel+1, (int)MAX_LEVELS));
	#endif

	ZL_Vector inp = ZLV(
		((ZL_Input::Held(ZLK_RIGHT) || ZL_Input::Held(ZLK_D)) ? 1 : 0) - ((ZL_Input::Held(ZLK_LEFT) || ZL_Input::Held(ZLK_A)) ? 1 : 0),
		((ZL_Input::Held(ZLK_UP) || ZL_Input::Held(ZLK_W)) ? 1 : 0) - ((ZL_Input::Held(ZLK_DOWN) || ZL_Input::Held(ZLK_S)) ? 1 : 0)
	);

	float titlef = (!endtitletime ? 1.0f : 1.0f - ZL_Math::Clamp01(ZLSINCE(endtitletime)/500.0f));
	bool pressedspace = ZL_Input::Down(ZLK_SPACE);
	bool havebox = !!boxbody->constraintList;
	bool canattach = false;

	if (!havebox)
	{
		for (int i = COUNT_OF(knotbodies)*6/10; i != COUNT_OF(knotbodies); i++)
		{
			cpVect knotpos = cpBodyGetPosition(knotbodies[i]);
			cpPointQueryInfo qi;
			if (cpShapePointQuery(boxbody->shapeList, knotpos, &qi) < 5)
			{
				canattach = true;
				break;
			}
		}
	}

	if (pressedspace)
	{
		if (havebox)
		{
			cpSpaceRemoveConstraint(space, boxbody->constraintList);
			sndAttach.Play();
			detachtime = ZLTICKS;
			havebox = false;
		}
		else if (canattach)
		{
			cpPointQueryInfo qi;
			cpShapePointQuery(boxbody->shapeList, cpvadd(cpBodyGetPosition(boxbody), cpv(0, 1000)), &qi);

			cpVect ptonbox = cpBodyWorldToLocal(boxbody, qi.point);
			if (sabs(ptonbox.x) > sabs(ptonbox.y)) { ptonbox.y = 0; }
			else { ptonbox.x = 0; }
			cpVect pt = cpBodyLocalToWorld(boxbody, ptonbox);

			cpVect src = cpBodyGetPosition(knotbodies[0]);
			cpVect dt = cpvmult(cpvsub(pt, src), 1.0f / (COUNT_OF(knotbodies)-1));
			for (int i = 1; i != COUNT_OF(knotbodies); i++)
			{
				cpBodySetPosition(knotbodies[i], src + cpvmult(dt, (float)i));
			}

			cpConstraint* joint = cpPivotJointNew(knotbodies[COUNT_OF(knotbodies)-1], boxbody, pt);
			cpSpaceAddConstraint(space, joint);
			sndAttach.Play();
			attachtime = ZLTICKS;
			havebox = true;
		}
	}

	static ticks_t TICKSUM = 0;
	for (TICKSUM += ZLELAPSEDTICKS; TICKSUM > 16; TICKSUM -= 16)
	{
		if (dietime || wintime) continue;
		float upv = 200.0f*inp.y;
		float turnv = 50.0f * inp.x
			+ 30.0f * cpBodyGetAngularVelocity(helibody) // magic
			+ 30.0f * ZL_Math::AngleSpread(cpBodyGetAngle(helibody)) // also magic
		;

		if (upv)
		{
			cpBodyApplyImpulseAtLocalPoint(helibody, cpv(0, upv), cpvzero);
		}
		else
		{
			// magical horizontal heli brake when not going up
			cpVect helivel = cpBodyGetVelocity(helibody);
			helivel.x *= 0.99f;
			cpBodySetVelocity(helibody, helivel);
		}

		if (turnv)
		{
			cpBodyApplyImpulseAtLocalPoint(helibody, cpv(turnv, 0), cpv(0, 15));
		}

		for (int i = 0; i != COUNT_OF(knotbodies); i++)
		{
			cpBodySetVelocity(knotbodies[i], cpvlerp(cpBodyGetVelocity(knotbodies[i]), cpBodyGetVelocity(helibody), .02f));
		}

		cpVect helipos = cpBodyGetPosition(helibody);

		if (havebox) 
		{
			// magical box x velocity clamping to avoid it swinging uncontrollably
			cpVect boxpos = cpBodyGetPosition(boxbody);
			cpVect helivel = cpBodyGetVelocity(helibody), boxvel = cpBodyGetVelocity(boxbody);
			float clampvelx = ZL_Math::Clamp(boxvel.x, (helivel.x < 0 ? helivel.x-100 : -50), (helivel.x > 0 ? helivel.x+100 : 50));
			
			// magical box horizontal movement to stay under the heli
			boxvel.x += (helipos.x - boxpos.x)*.01f;
			cpBodySetVelocity(boxbody, boxvel);

			// magical box rotation stabilization
			float boxturnv = 1.0f * cpBodyGetAngularVelocity(boxbody);
			cpBodyApplyImpulseAtLocalPoint(boxbody, cpv(-boxturnv, 0), cpv(0, -15));
		}
		else
		{
			cpShape* goalshape;
			for (int i = 0; i != COUNT_OF(padshapes) && padshapes[i];  i++) goalshape = padshapes[i];
			bool winning = (!havebox && sabs(boxbody->w) < 0.1 && sabs(boxbody->v.x) < 0.1 && sabs(boxbody->v.y) < 0.1 && cpShapesCollide(boxbody->shapeList, goalshape).count);
			winning = winning && ZL_Math::Abs((((int)(ZL_Math::Angle(cpBodyGetAngle(boxbody))*PIUNDER180) + 45) % 90) - 45) < 5;
			bool losing = (!winning && boxbody->p.y < waterlevel-15);
			wincount = (winning ? wincount + 1 : 0);
			losecount = (losing ? losecount + 1 : 0);

			if (losecount > 60) ResetBox();
		}

		for (int i = 0; i != COUNT_OF(turretbaseshapes);  i++)
		{
			if (!turretbaseshapes[i]) break;
			if (ZLSINCE(lastbullet[i]) > 5000 && sabs(helipos.x - turretpos[i].x) < 500)
			{
				cpVect turret2heli = cpvnormalize(cpvsub(helipos, turretpos[i]));
				cpBody *b = cpSpaceAddBody(space, cpBodyNew(10, cpMomentForCircle(10, 0, 5, cpvzero)));
				cpShape* shape = cpSpaceAddShape(space, cpCircleShapeNew(b, 5, cpvzero));
				cpBodySetUserData(b, (cpDataPointer)100);
				cpBodySetPosition(b, cpvadd(turretpos[i], cpvmult(turret2heli, 50)));
				cpShapeSetCollisionType(shape, COLLISION_MISSILE);
				cpBodySetVelocity(b, cpvmult(turret2heli, 100));
				missiles.push_back(b);
				lastbullet[i] = ZLTICKS;
			}
		}

		cpVect helivelg = cpvsub(cpBodyGetVelocity(helibody), ggravity);
		for (size_t i = 0; i < missiles.size(); i++)
		{
			cpBody* b = missiles[i];
			size_t gas = (size_t)cpBodyGetUserData(b);
			if (!gas)
			{
				particleFire.Spawn((int)20, cpBodyGetPosition(b));
				sndBoom.SetVolume(0.6f).Play();
				cpSpaceRemoveShape(space, b->shapeList);
				cpSpaceRemoveBody(space, b);
				missiles[i] = missiles.back();
				missiles.pop_back();
				continue;
			}
			cpBodySetUserData(b, (cpDataPointer)(gas-1));
			cpVect m2h = cpvsub(helipos, cpBodyGetPosition(b));
			float m2hdist = cpvlength(m2h);
			m2h = cpvadd(m2h, cpvmult(helivelg, m2hdist*.003f));

			m2h = cpvnormalize(m2h);
			cpBodySetForce(b, cpvmult(m2h, 5000));
		}

		cpSpaceStep(space, s(16.0/1000.0));
	}

	const ZL_Color* sky = skycolors[curlevel-1];
	ZL_Display::FillGradient(0, 0, ZLWIDTH, ZLHEIGHT, sky[0], sky[0], sky[1], sky[1]);

	cpVect pos = cpBodyGetPosition(helibody);
	float ar = ZL_Display::Width / ZL_Display::Height, yctr = 500, yvw = 300;
	if (titlef)
	{
		yctr = ZL_Math::Lerp(500, pos.y+20, titlef), yvw = ZL_Math::Lerp(300, 100, titlef);
		if (!endtitletime && (helibody->v.y > 20.0f || pressedspace))
		{
			endtitletime = ZLTICKS;
			levelstart = ZLTICKS;
			imcHeli.Play();
		}
		imcMusic.SetSongVolume((int)(15+15*titlef));
	}

	float viewl = pos.x - yvw * ar, viewr = pos.x + yvw * ar;
	ZL_Display::PushOrtho(viewl, viewr, yctr - yvw,  yctr + yvw);

	if (!dietime && !wintime && helibody->p.y < waterlevel-15)
	{
		particleFire.Spawn(50, cpBodyGetPosition(helibody));
		sndBoom.SetVolume(1.0f).Play();
		dietime = ZLTICKS;
		imcHeli.Stop();
	}

	if (!dietime)
	{
		ZL_Vector helip = cpBodyGetPosition(helibody);
		srfHeli.Draw(helip, cpBodyGetAngle(helibody));
		imcHeli.SetSongBPM(128 + cpvlength(helibody->v));
		ZL_Vector heliv = ZL_Vector::FromAngle(cpBodyGetAngle(helibody));
		ZL_Vector heliw = heliv.VecPerp();
		cpVect helivel = cpBodyGetVelocity(helibody);
		float heliangvel = cpBodyGetAngularVelocity(helibody);
		static float rotort, tailt;
		rotort += ZLELAPSEDF(20.0f + sabs(helivel.y)*.01f);
		float rotoranim = ssin(rotort) * 40;
		ZL_Display::DrawWideLine(helip + heliw*13 + heliv*-rotoranim, helip + heliw*15 + heliv*rotoranim, 0.5f, ZLTRANSPARENT, ZLBLACK);

		tailt += ZLELAPSEDF(20.0f + sabs(heliangvel)*1.1f);
		ZL_Vector tail1 = ZL_Vector::FromAngle(tailt+cpBodyGetAngle(helibody)+PI2*.00000f);
		ZL_Vector tail2 = ZL_Vector::FromAngle(tailt+cpBodyGetAngle(helibody)+PI2*.33333f);
		ZL_Vector tail3 = ZL_Vector::FromAngle(tailt+cpBodyGetAngle(helibody)+PI2*.66666f);
		ZL_Vector tailp = helip + heliv*-40;
		ZL_Display::DrawWideLine(tailp, tailp + tail1*10, 0.5f, ZLTRANSPARENT, ZLBLACK);
		ZL_Display::DrawWideLine(tailp, tailp + tail2*10, 0.5f, ZLTRANSPARENT, ZLBLACK);
		ZL_Display::DrawWideLine(tailp, tailp + tail3*10, 0.5f, ZLTRANSPARENT, ZLBLACK);

		ZL_Vector knotp = helip - heliw*15;
		for (int i = 0; i != COUNT_OF(knotbodies) && endtitletime; i++)
		{
			ZL_Vector knotp2 = cpBodyGetPosition(knotbodies[i]);
			ZL_Display::DrawLine(knotp.x + 1, knotp.y + 1, knotp2.x + 1, knotp2.y + 1, ZL_Color::Black);
			ZL_Display::DrawLine(knotp, knotp2, ZL_Color::Orange);
			knotp = knotp2;
		}
	}

	for (cpBody* b : missiles)
	{
		srfMissile.Draw(ZL_Vector(b->p), cpvtoangle(cpvnormalize(cpBodyGetVelocity(b))));
	}

	for (int i = 0; i != COUNT_OF(padshapes);  i++)
	{
		cpPolyShape *poly = (cpPolyShape *)padshapes[i];
		if (!poly) break;
		srfCement.DrawQuad(poly->planes[0].v0, poly->planes[1].v0, poly->planes[2].v0, poly->planes[3].v0);
		ZL_Display::DrawQuad(poly->planes[0].v0, poly->planes[1].v0, poly->planes[2].v0, poly->planes[3].v0, ZL_Color::DarkGray);
	}

	for (int i = 0; i != COUNT_OF(turretbaseshapes);  i++)
	{
		cpPolyShape *poly = (cpPolyShape *)turretbaseshapes[i];
		if (!poly) break;
		ZL_Display::DrawQuad(poly->planes[0].v0, poly->planes[1].v0, poly->planes[2].v0, poly->planes[3].v0, ZL_Color::DarkGray, ZL_Color::DarkRed);
		ZL_Vector gunp = turretpos[i];
		ZL_Vector gunv = (ZL_Vector(cpBodyGetPosition(helibody)) - gunp).VecNorm() * 20.0f;
		ZL_Vector gunw = gunv.VecPerp() * 0.25f;
		ZL_Display::DrawQuad(gunp + gunv + gunw, gunp - gunv + gunw, gunp - gunv - gunw, gunp + gunv - gunw, ZL_Color::DarkGray, ZL_Color::Red);
	}

	ZL_Vector boxp = cpBodyGetPosition(boxbody);
	ZL_Vector boxv = ZL_Vector::FromAngle(cpBodyGetAngle(boxbody)) * 15.f;
	ZL_Vector boxw = boxv.VecPerp();
	srfBox.DrawQuad(boxp - boxv + boxw, boxp + boxv + boxw, boxp + boxv - boxw, boxp - boxv - boxw);

	ZL_Color colorland = landcolors[curlevel-1];
	for (int i = 0; i != COUNT_OF(heightmap)-1; i++)
	{
		float x2 = lsx(i+1);
		if (x2 < viewl) continue;
		float x1 = lsx(i), y1 = heightmap[i], y2 = heightmap[i+1];
		if (x1 > viewr) break;
		ZL_Display::FillQuad(x1, y1, x2, y2, x2, 0, x1, 0, colorland);
		ZL_Display::DrawLine(x1, y1, x2, y2, ZLBLACK);
	}

	ZL_Display::FillRect(viewl, waterlevel, viewr, 0, watercolor);

	particleFire.Draw();

	#ifdef ZILLALOG //DEBUG DRAW
	if (ZL_Input::Held(ZLK_LSHIFT))
	{
		void DebugDrawConstraint(cpConstraint*, void*); cpSpaceEachConstraint(space, DebugDrawConstraint, NULL);
		void DebugDrawShape(cpShape*,void*); cpSpaceEachShape(space, DebugDrawShape, NULL);
	}
	#endif

	cpVect helipos = cpBodyGetPosition(helibody);
	cpVect trgpos = (havebox ? goalpos : cpBodyGetPosition(boxbody));
	cpVect heli2trg = cpvsub(trgpos, helipos);
	if (!wincount && !dietime && endtitletime)
	{
		float heli2trgdist = cpvlength(heli2trg);
		cpVect heli2trgdir = cpvmult(heli2trg, 1.0f/(heli2trgdist + CPFLOAT_MIN));
		float heli2trga = cpvtoangle(heli2trg);
		cpVect arrowpos = cpvlerp(cpvsub(trgpos, cpvmult(heli2trgdir, 40)), cpvadd(helipos, cpvmult(heli2trgdir, 50)), ZL_Math::Clamp01((heli2trgdist - 200) / 200));
		srfArrow.Draw(arrowpos, heli2trga);
	}

	ZL_Display::PopMatrix();

	if (titlef)
	{
		static ZL_TextBuffer txt(fntBig, "SUPPLY");
		ZL_Color coltxt = ZLLUMA(1, titlef), colttl = ZLRGBA(1, 0.7, 0, titlef), colbrdr = ZLLUMA(0,titlef*titlef);
		ZL_Display::PushMatrix();
		ZL_Display::Rotate(0.3f);
		DrawTextBordered(txt, ZLV(ZLHALFW-80, ZLHALFH+0), 2.0f, ZLLUMA(0, titlef*.2f), ZLLUMA(0, titlef*.2f), 4);
		DrawTextBordered(txt, ZLV(ZLHALFW-100, ZLHALFH+20), 2.0f, colttl, colbrdr, 4);
		ZL_Display::PopMatrix();
		static ZL_TextBuffer txt2(fntBig, "DROP");
		for (float i = 0.1f; i < 0.2f; i += 0.04f)
			DrawTextBordered(txt2, ZLV(ZLHALFW+250, ZLHALFH+200-i*600), 2.0f+i, ZLLUMA(0, titlef*i), ZLLUMA(0, titlef*i), 4);
		DrawTextBordered(txt2, ZLV(ZLHALFW+250, ZLHALFH+200-130), 2.3f, colttl, colbrdr, 4);
		
		static ZL_TextBuffer txt3(fntMain, "USE ARROWS OR WASD TO MOVE");
		DrawTextBordered(txt3, ZLV(ZLHALFW, ZLHALFH-180), .8f, coltxt, colbrdr, 4);
		static ZL_TextBuffer txt4(fntMain, "USE SPACE TO ATTACH THE ROPE");
		DrawTextBordered(txt4, ZLV(ZLHALFW, ZLHALFH-215), .8f, coltxt, colbrdr, 4);
		static ZL_TextBuffer txt5(fntMain, "MOVE UP TO START");
		DrawTextBordered(txt5, ZLV(ZLHALFW, ZLHALFH-250), .8f, coltxt, colbrdr, 4);

		static ZL_TextBuffer txt6(fntMain, "- 2023 - BERNHARD SCHELLING -");
		DrawTextBordered(txt6, ZLV(ZLHALFW, 20), .7f, coltxt*.7f, colbrdr, 2);
	}

	if (wincount > 60 || wintime)
	{
		if (curlevel == MAX_LEVELS)
		{
			static ZL_TextBuffer txt(fntBig, "THANK YOU FOR PLAYING!");
			DrawTextBordered(txt, ZLV(ZLHALFW, ZLHALFH-100*.8f), .8f, ZLWHITE, ZLBLACK, 3);
			if (!wintime) { wintime = ZLTICKS; sndWin.Play(); imcHeli.Stop(); }
			if (ZLSINCE(wintime) > 500 && pressedspace) Init();
		}
		else
		{
			static ZL_TextBuffer txt(fntBig, "CONGRATULATIONS!");
			DrawTextBordered(txt, ZLV(ZLHALFW, ZLHALFH-100*.8f), .8f, ZLWHITE, ZLBLACK, 3);
			if (!wintime) { wintime = ZLTICKS; sndWin.Play(); imcHeli.Stop(); }
			if (ZLSINCE(wintime) > 2000) Init(curlevel + 1);
		}
	}
	else if (dietime)
	{
		static ZL_TextBuffer txt(fntMain, "PRESS SPACE TO RESPAWN");
		DrawTextBordered(txt, ZLV(ZLHALFW, ZLHALFH-100*.8f), .8f);
		if (pressedspace) Init(curlevel);
	}
	else if (havebox && sabs(heli2trg.x) < 100)
	{
		static ZL_TextBuffer txt(fntMain, "PRESS SPACE TO DROP");
		DrawTextBordered(txt, ZLV(ZLHALFW, ZLHALFH-100*.8f), .8f);
	}
	else if (canattach && !wincount && (!detachtime || ZLSINCE(detachtime) > 1000))
	{
		static ZL_TextBuffer txt(fntMain, "PRESS SPACE TO ATTACH");
		DrawTextBordered(txt, ZLV(ZLHALFW, ZLHALFH-100*.8f), .8f);
	}
	else if (curlevel == 1)
	{
		if (attachtime && ZLSINCE(attachtime) < 3000 && (!detachtime || ZLSINCE(detachtime) > 1000))
		{
			static ZL_TextBuffer txt(fntMain, "DELIVER THE BOX");
			DrawTextBordered(txt, ZLV(ZLHALFW, ZLHALFH-100*.8f), .8f);
		}
		else if (levelstart && ZLSINCE(levelstart) < 3000)
		{
			static ZL_TextBuffer txt(fntMain, "ATTACH THE BOX TO THE ROPE");
			DrawTextBordered(txt, ZLV(ZLHALFW, ZLHALFH-100*.8f), .8f);
		}
	}
	else if (curlevel == 2)
	{
		if (ZLSINCE(levelstart) < 3000)
		{
			static ZL_TextBuffer txt(fntMain, "WATCH OUT FOR MISSILES");
			DrawTextBordered(txt, ZLV(ZLHALFW, ZLHALFH-100*.8f), .8f);
		}
	}
	else if (ZLSINCE(levelstart) < 3000)
	{
		static ZL_TextBuffer txt(fntMain, "DELIVER THE BOX");
		DrawTextBordered(txt, ZLV(ZLHALFW, ZLHALFH-100*.8f), .8f);
	}
}

static struct sSupplyDrop : public ZL_Application
{
	sSupplyDrop() : ZL_Application(60) { }

	virtual void Load(int argc, char *argv[])
	{
		if (!ZL_Application::LoadReleaseDesktopDataBundle()) return;
		if (!ZL_Display::Init("Supply Drop", 1280, 720, ZL_DISPLAY_ALLOWRESIZEHORIZONTAL)) return;
		ZL_Display::ClearFill(ZL_Color::White);
		ZL_Display::SetAA(true);
		ZL_Audio::Init();
		ZL_Input::Init();
		::Load();
		::Init();
	}
	virtual void AfterFrame()
	{
		::Frame();
	}
} SupplyDrop;

#ifdef ZILLALOG //DEBUG DRAW
void DebugDrawShape(cpShape *shape, void*)
{
	switch (shape->klass->type)
	{
		case CP_CIRCLE_SHAPE: {
			cpCircleShape *circle = (cpCircleShape *)shape;
			ZL_Display::DrawCircle(circle->tc, circle->r, ZL_Color::Green);
			break; }
		case CP_SEGMENT_SHAPE: {
			cpSegmentShape *seg = (cpSegmentShape *)shape;
			cpVect vw = cpvclamp(cpvperp(cpvsub(seg->tb, seg->ta)), seg->r);
			ZL_Display::DrawQuad(seg->ta.x + vw.x, seg->ta.y + vw.y, seg->tb.x + vw.x, seg->tb.y + vw.y, seg->tb.x - vw.x, seg->tb.y - vw.y, seg->ta.x - vw.x, seg->ta.y - vw.y, ZLRGBA(0,1,1,.35), ZLRGBA(1,1,0,.35));
			ZL_Display::DrawCircle(seg->ta, seg->r, ZLRGBA(0,1,1,.35), ZLRGBA(1,1,0,.35));
			ZL_Display::DrawCircle(seg->tb, seg->r, ZLRGBA(0,1,1,.35), ZLRGBA(1,1,0,.35));
			break; }
		case CP_POLY_SHAPE: {
			cpPolyShape *poly = (cpPolyShape *)shape;
			{for (int i = 1; i < poly->count; i++) ZL_Display::DrawLine(poly->planes[i-1].v0, poly->planes[i].v0, ZLWHITE);}
			ZL_Display::DrawLine(poly->planes[poly->count-1].v0, poly->planes[0].v0, ZLWHITE);
			break; }
	}
	ZL_Display::FillCircle(cpBodyGetPosition(shape->body), 3, ZL_Color::Red);
	ZL_Display::DrawLine(cpBodyGetPosition(shape->body), (ZL_Vector&)cpBodyGetPosition(shape->body) + ZLV(cpBodyGetAngularVelocity(shape->body)*-10, 0), ZLRGB(1,0,0));
	ZL_Display::DrawLine(cpBodyGetPosition(shape->body), (ZL_Vector&)cpBodyGetPosition(shape->body) + ZL_Vector::FromAngle(cpBodyGetAngle(shape->body))*10, ZLRGB(1,1,0));
}

void DebugDrawConstraint(cpConstraint *constraint, void *data)
{
	cpBody *body_a = constraint->a, *body_b = constraint->b;

	if(cpConstraintIsPinJoint(constraint) || cpConstraintIsSlideJoint(constraint))
	{
		cpPinJoint *joint = (cpPinJoint *)constraint;
		cpVect a = (cpBodyGetType(body_a) == CP_BODY_TYPE_KINEMATIC ? body_a->p : cpTransformPoint(body_a->transform, joint->anchorA));
		cpVect b = (cpBodyGetType(body_b) == CP_BODY_TYPE_KINEMATIC ? body_b->p : cpTransformPoint(body_b->transform, joint->anchorB));
		ZL_Display::DrawLine(a.x, a.y, b.x, b.y, ZL_Color::Magenta);
	}
	else if (cpConstraintIsPivotJoint(constraint))
	{
		cpPivotJoint *joint = (cpPivotJoint *)constraint;
		cpVect a = (cpBodyGetType(body_a) == CP_BODY_TYPE_KINEMATIC ? body_a->p : cpTransformPoint(body_a->transform, joint->anchorA));
		cpVect b = (cpBodyGetType(body_b) == CP_BODY_TYPE_KINEMATIC ? body_b->p : cpTransformPoint(body_b->transform, joint->anchorB));
		ZL_Display::DrawLine(a.x, a.y, b.x, b.y, ZL_Color::Magenta);
		ZL_Display::FillCircle(a, 2, ZL_Color::Magenta);
		ZL_Display::FillCircle(b, 2, ZL_Color::Magenta);
	}
	else if (cpConstraintIsRotaryLimitJoint(constraint))
	{
		cpRotaryLimitJoint *joint = (cpRotaryLimitJoint *)constraint;
		cpVect a = cpTransformPoint(body_a->transform, cpvzero);
		cpVect b = cpvadd(a, cpvmult(cpvforangle(joint->min), 40));
		cpVect c = cpvadd(a, cpvmult(cpvforangle(joint->max), 40));
		ZL_Display::DrawLine(a.x, a.y, b.x, b.y, ZL_Color::Magenta);
		ZL_Display::DrawLine(a.x, a.y, c.x, c.y, ZL_Color::Magenta);
	}
}
#endif

#if /* MUSIC/SOUND FX */ 1
static const unsigned int IMCMUSIC_OrderTable[] = {
	0x011000001, 0x022000002, 0x011000003, 0x012100004, 0x011100004, 0x022000005, 0x021100005, 0x022000007,
	0x011100008, 0x012000007, 0x021100008, 0x022000000, 0x022100006, 0x001000002,
};
static const unsigned char IMCMUSIC_PatternData[] = {
	0x40, 0, 0x42, 0, 0x45, 0, 0x47, 0, 0x40, 0, 0x42, 0, 0x45, 0, 0x47, 0,
	0x50, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 255,
	0x54, 0x52, 0x50, 0, 0x52, 0x50, 0x4B, 0, 0x50, 0x4B, 0x49, 0, 0, 0, 0x49, 0,
	0x44, 0, 0, 0x44, 0x42, 0, 0x40, 0, 0x42, 0, 0x42, 0x40, 0x3B, 0, 0, 0,
	0x40, 0, 0x40, 0, 0x44, 0, 0x40, 0, 0x3B, 0, 0x42, 0, 0x44, 0, 0x45, 0,
	0x45, 0, 0, 0, 0x42, 0, 0, 0, 0x45, 0, 0, 0, 0x49, 0, 0, 0,
	0x40, 0, 0, 0, 0x45, 0, 0x44, 0, 0x40, 0, 0, 0, 0, 0, 0, 0,
	0x44, 0, 0, 0, 0x47, 0, 0x45, 0, 0x44, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0x30, 0, 0x47, 0, 0x30, 0, 0, 0,
	0x12, 0, 0x12, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0x20, 0, 0x20, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0x47, 0, 0, 0, 0x47, 0, 0, 0, 0x47, 0, 0, 0, 0x47, 0, 0, 0,
	0x47, 0, 0x47, 0, 0x47, 0, 0x47, 0, 0x47, 0, 0x47, 0, 0x47, 0, 0x47, 0,
};
static const unsigned char IMCMUSIC_PatternLookupTable[] = { 0, 8, 8, 8, 8, 8, 9, 11, };
static const TImcSongEnvelope IMCMUSIC_EnvList[] = {
	{ 0, 256, 64, 8, 16, 4, true, 255, },
	{ 0, 256, 261, 8, 16, 255, true, 255, },
	{ 0, 256, 379, 8, 16, 255, true, 255, },
	{ 32, 256, 196, 8, 16, 255, true, 255, },
	{ 0, 256, 64, 8, 16, 0, true, 255, },
	{ 50, 100, 130, 24, 255, 255, true, 255, },
	{ 25, 256, 61, 8, 16, 255, true, 255, },
	{ 0, 256, 87, 8, 16, 255, true, 255, },
	{ 196, 256, 29, 8, 16, 255, true, 255, },
	{ 0, 128, 1046, 8, 16, 255, true, 255, },
};
static TImcSongEnvelopeCounter IMCMUSIC_EnvCounterList[] = {
	{ 0, 0, 256 }, { -1, -1, 256 }, { 1, 0, 256 }, { 2, 5, 256 },
	{ 3, 5, 256 }, { 4, 6, 256 }, { 5, 6, 50 }, { 6, 6, 256 },
	{ 7, 7, 256 }, { 8, 7, 256 }, { 7, 7, 256 }, { 9, 7, 128 },
};
static const TImcSongOscillator IMCMUSIC_OscillatorList[] = {
	{ 7, 0, IMCSONGOSCTYPE_SQUARE, 0, -1, 124, 1, 1 },
	{ 7, 0, IMCSONGOSCTYPE_SQUARE, 0, -1, 124, 1, 1 },
	{ 8, 1, IMCSONGOSCTYPE_SQUARE, 0, -1, 158, 1, 1 },
	{ 9, 33, IMCSONGOSCTYPE_SQUARE, 0, -1, 86, 2, 1 },
	{ 8, 125, IMCSONGOSCTYPE_SQUARE, 0, 0, 22, 1, 1 },
	{ 8, 125, IMCSONGOSCTYPE_SQUARE, 0, 1, 22, 1, 1 },
	{ 8, 0, IMCSONGOSCTYPE_SINE, 0, 2, 22, 1, 1 },
	{ 9, 0, IMCSONGOSCTYPE_SINE, 0, 3, 28, 1, 1 },
	{ 8, 0, IMCSONGOSCTYPE_NOISE, 5, -1, 127, 1, 4 },
	{ 8, 0, IMCSONGOSCTYPE_SAW, 6, -1, 100, 1, 1 },
	{ 7, 0, IMCSONGOSCTYPE_SQUARE, 6, -1, 64, 1, 1 },
	{ 7, 0, IMCSONGOSCTYPE_SINE, 6, -1, 255, 1, 1 },
	{ 8, 0, IMCSONGOSCTYPE_SAW, 6, 9, 24, 1, 1 },
	{ 9, 0, IMCSONGOSCTYPE_SQUARE, 6, 10, 10, 1, 1 },
	{ 5, 15, IMCSONGOSCTYPE_SINE, 7, -1, 98, 8, 9 },
	{ 7, 0, IMCSONGOSCTYPE_SINE, 7, -1, 200, 10, 11 },
};
static const TImcSongEffect IMCMUSIC_EffectList[] = {
	{ 74, 0, 1, 0, IMCSONGEFFECTTYPE_LOWPASS, 1, 0 },
	{ 142, 193, 1, 0, IMCSONGEFFECTTYPE_RESONANCE, 1, 1 },
	{ 128, 0, 5952, 5, IMCSONGEFFECTTYPE_DELAY, 0, 0 },
	{ 255, 110, 1, 5, IMCSONGEFFECTTYPE_RESONANCE, 1, 1 },
	{ 227, 0, 1, 5, IMCSONGEFFECTTYPE_HIGHPASS, 1, 0 },
	{ 0, 0, 101, 6, IMCSONGEFFECTTYPE_FLANGE, 6, 0 },
	{ 202, 165, 1, 6, IMCSONGEFFECTTYPE_RESONANCE, 7, 1 },
	{ 5207, 1604, 1, 7, IMCSONGEFFECTTYPE_OVERDRIVE, 0, 1 },
};
static unsigned char IMCMUSIC_ChannelVol[8] = { 100, 128, 100, 100, 100, 192, 122, 225 };
static const unsigned char IMCMUSIC_ChannelEnvCounter[8] = { 0, 0, 0, 0, 0, 3, 5, 1 };
static const bool IMCMUSIC_ChannelStopNote[8] = { false, false, false, false, false, true, false, true };
TImcSongData imcDataIMCMUSIC = {
	/*LEN*/ 0xE, /*ROWLENSAMPLES*/ 5088, /*ENVLISTSIZE*/ 10, /*ENVCOUNTERLISTSIZE*/ 12, /*OSCLISTSIZE*/ 16, /*EFFECTLISTSIZE*/ 8, /*VOL*/ 26,
	IMCMUSIC_OrderTable, IMCMUSIC_PatternData, IMCMUSIC_PatternLookupTable, IMCMUSIC_EnvList, IMCMUSIC_EnvCounterList, IMCMUSIC_OscillatorList, IMCMUSIC_EffectList,
	IMCMUSIC_ChannelVol, IMCMUSIC_ChannelEnvCounter, IMCMUSIC_ChannelStopNote };
ZL_SynthImcTrack imcMusic(&imcDataIMCMUSIC);

static const unsigned int IMCHELI_OrderTable[] = {
	0x000000001,
};
static const unsigned char IMCHELI_PatternData[] = {
	0x40, 0, 0x40, 0, 0x40, 0, 0x40, 0, 0x40, 0, 0x40, 0, 0x40, 0, 0x40, 0,
};
static const unsigned char IMCHELI_PatternLookupTable[] = { 0, 1, 1, 1, 1, 1, 1, 1, };
static const TImcSongEnvelope IMCHELI_EnvList[] = {
	{ 0, 256, 204, 8, 16, 4, true, 255, },
	{ 64, 256, 209, 8, 16, 255, true, 255, },
	{ 64, 256, 173, 8, 15, 255, true, 255, },
	{ 0, 256, 3226, 8, 16, 255, true, 255, },
	{ 128, 196, 8, 8, 255, 255, false, 255, },
};
static TImcSongEnvelopeCounter IMCHELI_EnvCounterList[] = {
	{ 0, 0, 256 }, { 1, 0, 256 }, { 2, 0, 256 }, { -1, -1, 256 },
	{ 3, 0, 256 }, { 4, 0, 196 },
};
static const TImcSongOscillator IMCHELI_OscillatorList[] = {
	{ 6, 127, IMCSONGOSCTYPE_SINE, 0, -1, 186, 1, 2 },
	{ 8, 0, IMCSONGOSCTYPE_NOISE, 0, -1, 124, 4, 3 },
	{ 8, 85, IMCSONGOSCTYPE_NOISE, 0, 0, 72, 3, 3 },
	{ 8, 0, IMCSONGOSCTYPE_SINE, 1, -1, 100, 0, 0 },
	{ 8, 0, IMCSONGOSCTYPE_SINE, 2, -1, 100, 0, 0 },
	{ 8, 0, IMCSONGOSCTYPE_SINE, 3, -1, 100, 0, 0 },
	{ 8, 0, IMCSONGOSCTYPE_SINE, 4, -1, 100, 0, 0 },
	{ 8, 0, IMCSONGOSCTYPE_SINE, 5, -1, 100, 0, 0 },
	{ 8, 0, IMCSONGOSCTYPE_SINE, 6, -1, 100, 0, 0 },
	{ 8, 0, IMCSONGOSCTYPE_SINE, 7, -1, 100, 0, 0 },
};
static const TImcSongEffect IMCHELI_EffectList[] = {
	{ 0, 0, 197, 0, IMCSONGEFFECTTYPE_FLANGE, 5, 0 },
	{ 90, 144, 1, 0, IMCSONGEFFECTTYPE_RESONANCE, 3, 3 },
};
static unsigned char IMCHELI_ChannelVol[8] = { 71, 100, 100, 100, 100, 100, 100, 100 };
static const unsigned char IMCHELI_ChannelEnvCounter[8] = { 0, 0, 0, 0, 0, 0, 0, 0 };
static const bool IMCHELI_ChannelStopNote[8] = { false, false, false, false, false, false, false, false };
TImcSongData imcDataIMCHELI = {
	/*LEN*/ 0x1, /*ROWLENSAMPLES*/ 2594, /*ENVLISTSIZE*/ 5, /*ENVCOUNTERLISTSIZE*/ 6, /*OSCLISTSIZE*/ 10, /*EFFECTLISTSIZE*/ 2, /*VOL*/ 100,
	IMCHELI_OrderTable, IMCHELI_PatternData, IMCHELI_PatternLookupTable, IMCHELI_EnvList, IMCHELI_EnvCounterList, IMCHELI_OscillatorList, IMCHELI_EffectList,
	IMCHELI_ChannelVol, IMCHELI_ChannelEnvCounter, IMCHELI_ChannelStopNote };
ZL_SynthImcTrack imcHeli(&imcDataIMCHELI);

static const unsigned int IMCATTACH_OrderTable[] = { 0x000000001, };
static const unsigned char IMCATTACH_PatternData[] = { 0x50, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, };
static const unsigned char IMCATTACH_PatternLookupTable[] = { 0, 1, 1, 1, 1, 1, 1, 1, };
static const TImcSongEnvelope IMCATTACH_EnvList[] = {
	{ 0, 256, 69, 8, 16, 255, true, 255, },
	{ 40, 155, 69, 25, 15, 255, true, 255, },
	{ 0, 256, 184, 5, 19, 255, true, 255, },
};
static TImcSongEnvelopeCounter IMCATTACH_EnvCounterList[] = {
	{ 0, 0, 256 }, { -1, -1, 256 }, { 1, 0, 40 }, { 2, 0, 238 },
};
static const TImcSongOscillator IMCATTACH_OscillatorList[] = {
	{ 6, 221, IMCSONGOSCTYPE_SINE, 0, -1, 80, 1, 2 },
	{ 8, 0, IMCSONGOSCTYPE_NOISE, 0, 0, 90, 3, 1 },
	{ 8, 0, IMCSONGOSCTYPE_SINE, 1, -1, 100, 0, 0 },
	{ 8, 0, IMCSONGOSCTYPE_SINE, 2, -1, 100, 0, 0 },
	{ 8, 0, IMCSONGOSCTYPE_SINE, 3, -1, 100, 0, 0 },
	{ 8, 0, IMCSONGOSCTYPE_SINE, 4, -1, 100, 0, 0 },
	{ 8, 0, IMCSONGOSCTYPE_SINE, 5, -1, 100, 0, 0 },
	{ 8, 0, IMCSONGOSCTYPE_SINE, 6, -1, 100, 0, 0 },
	{ 8, 0, IMCSONGOSCTYPE_SINE, 7, -1, 100, 0, 0 },
};
static const TImcSongEffect IMCATTACH_EffectList[] = {
	{ 16891, 337, 1, 0, IMCSONGEFFECTTYPE_OVERDRIVE, 0, 1 },
};
static unsigned char IMCATTACH_ChannelVol[8] = { 102, 100, 100, 100, 100, 100, 100, 100 };
static const unsigned char IMCATTACH_ChannelEnvCounter[8] = { 0, 0, 0, 0, 0, 0, 0, 0 };
static const bool IMCATTACH_ChannelStopNote[8] = { true, false, false, false, false, false, false, false };
TImcSongData imcDataIMCATTACH = {
	/*LEN*/ 0x1, /*ROWLENSAMPLES*/ 2594, /*ENVLISTSIZE*/ 3, /*ENVCOUNTERLISTSIZE*/ 4, /*OSCLISTSIZE*/ 9, /*EFFECTLISTSIZE*/ 1, /*VOL*/ 128,
	IMCATTACH_OrderTable, IMCATTACH_PatternData, IMCATTACH_PatternLookupTable, IMCATTACH_EnvList, IMCATTACH_EnvCounterList, IMCATTACH_OscillatorList, IMCATTACH_EffectList,
	IMCATTACH_ChannelVol, IMCATTACH_ChannelEnvCounter, IMCATTACH_ChannelStopNote };

static const unsigned int IMCHIT_OrderTable[] = { 0x000000001, };
static const unsigned char IMCHIT_PatternData[] = { 0x49, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, };
static const unsigned char IMCHIT_PatternLookupTable[] = { 0, 1, 1, 1, 1, 1, 1, 1, };
static const TImcSongEnvelope IMCHIT_EnvList[] = {
	{ 0, 256, 204, 8, 16, 4, true, 255, },
	{ 0, 256, 209, 8, 16, 255, true, 255, },
	{ 64, 256, 261, 8, 15, 255, true, 255, },
	{ 0, 256, 3226, 8, 16, 255, true, 255, },
};
static TImcSongEnvelopeCounter IMCHIT_EnvCounterList[] = {
	{ 0, 0, 256 }, { 1, 0, 256 }, { 2, 0, 256 }, { -1, -1, 256 },
	{ 3, 0, 256 },
};
static const TImcSongOscillator IMCHIT_OscillatorList[] = {
	{ 6, 127, IMCSONGOSCTYPE_SINE, 0, -1, 206, 1, 2 },
	{ 8, 0, IMCSONGOSCTYPE_NOISE, 0, -1, 186, 4, 3 },
	{ 7, 0, IMCSONGOSCTYPE_NOISE, 0, 0, 152, 3, 3 },
};
static const TImcSongEffect IMCHIT_EffectList[] = {
	{ 9906, 843, 1, 0, IMCSONGEFFECTTYPE_OVERDRIVE, 0, 3 },
	{ 41, 51, 1, 0, IMCSONGEFFECTTYPE_RESONANCE, 3, 3 },
};
static unsigned char IMCHIT_ChannelVol[8] = { 71, 128, 100, 100, 100, 100, 100, 100 };
static const unsigned char IMCHIT_ChannelEnvCounter[8] = { 0, 0, 0, 0, 0, 0, 0, 0 };
static const bool IMCHIT_ChannelStopNote[8] = { false, true, false, false, false, false, false, false };
TImcSongData imcDataIMCHIT = {
	/*LEN*/ 0x1, /*ROWLENSAMPLES*/ 5512, /*ENVLISTSIZE*/ 4, /*ENVCOUNTERLISTSIZE*/ 5, /*OSCLISTSIZE*/ 3, /*EFFECTLISTSIZE*/ 2, /*VOL*/ 100,
	IMCHIT_OrderTable, IMCHIT_PatternData, IMCHIT_PatternLookupTable, IMCHIT_EnvList, IMCHIT_EnvCounterList, IMCHIT_OscillatorList, IMCHIT_EffectList,
	IMCHIT_ChannelVol, IMCHIT_ChannelEnvCounter, IMCHIT_ChannelStopNote };

static const unsigned int IMCBOOM_OrderTable[] = {
	0x000000001,
};
static const unsigned char IMCBOOM_PatternData[] = {
	0x40, 0x42, 0x44, 0x45, 0x47, 0x49, 0x4B, 0x50, 0x52, 0x40, 255, 0, 0, 0, 0, 0,
};
static const unsigned char IMCBOOM_PatternLookupTable[] = { 0, 1, 1, 1, 1, 1, 1, 1, };
static const TImcSongEnvelope IMCBOOM_EnvList[] = {
	{ 0, 256, 78, 8, 16, 0, true, 255, },
	{ 0, 256, 5, 0, 24, 255, true, 255, },
	{ 0, 256, 42, 8, 16, 255, true, 255, },
	{ 0, 256, 87, 5, 19, 255, true, 255, },
	{ 0, 256, 69, 27, 255, 255, false, 0, },
};
static TImcSongEnvelopeCounter IMCBOOM_EnvCounterList[] = {
	{ 0, 0, 256 }, { 1, 0, 128 }, { 2, 0, 256 }, { 3, 0, 238 },
	{ -1, -1, 256 }, { 4, 0, 18 },
};
static const TImcSongOscillator IMCBOOM_OscillatorList[] = {
	{ 6, 169, IMCSONGOSCTYPE_SQUARE, 0, -1, 80, 1, 2 },
	{ 8, 0, IMCSONGOSCTYPE_NOISE, 0, 0, 90, 3, 4 },
	{ 8, 0, IMCSONGOSCTYPE_SINE, 1, -1, 100, 0, 0 },
	{ 8, 0, IMCSONGOSCTYPE_SINE, 2, -1, 100, 0, 0 },
	{ 8, 0, IMCSONGOSCTYPE_SINE, 3, -1, 100, 0, 0 },
	{ 8, 0, IMCSONGOSCTYPE_SINE, 4, -1, 100, 0, 0 },
	{ 8, 0, IMCSONGOSCTYPE_SINE, 5, -1, 100, 0, 0 },
	{ 8, 0, IMCSONGOSCTYPE_SINE, 6, -1, 100, 0, 0 },
	{ 8, 0, IMCSONGOSCTYPE_SINE, 7, -1, 100, 0, 0 },
};
static const TImcSongEffect IMCBOOM_EffectList[] = {
	{ 9652, 668, 1, 0, IMCSONGEFFECTTYPE_OVERDRIVE, 0, 4 },
	{ 105, 100, 1, 0, IMCSONGEFFECTTYPE_RESONANCE, 5, 4 },
};
static unsigned char IMCBOOM_ChannelVol[8] = { 99, 100, 100, 100, 100, 100, 100, 100 };
static const unsigned char IMCBOOM_ChannelEnvCounter[8] = { 0, 0, 0, 0, 0, 0, 0, 0 };
static const bool IMCBOOM_ChannelStopNote[8] = { false, false, false, false, false, false, false, false };
TImcSongData imcDataIMCBOOM = {
	/*LEN*/ 0x1, /*ROWLENSAMPLES*/ 2966, /*ENVLISTSIZE*/ 5, /*ENVCOUNTERLISTSIZE*/ 6, /*OSCLISTSIZE*/ 9, /*EFFECTLISTSIZE*/ 2, /*VOL*/ 100,
	IMCBOOM_OrderTable, IMCBOOM_PatternData, IMCBOOM_PatternLookupTable, IMCBOOM_EnvList, IMCBOOM_EnvCounterList, IMCBOOM_OscillatorList, IMCBOOM_EffectList,
	IMCBOOM_ChannelVol, IMCBOOM_ChannelEnvCounter, IMCBOOM_ChannelStopNote };

static const unsigned int IMCWIN_OrderTable[] = { 0x000000001, };
static const unsigned char IMCWIN_PatternData[] = { 0x22, 0x24, 0x25, 0, 0x29, 0x27, 0x25, 0, 0x30, 0x32, 0x34, 0x35, 0x37, 0, 0, 0, };
static const unsigned char IMCWIN_PatternLookupTable[] = { 0, 1, 1, 1, 1, 1, 1, 1, };
static const TImcSongEnvelope IMCWIN_EnvList[] = {
	{ 0, 256, 64, 8, 16, 3, true, 255, },
	{ 0, 256, 261, 8, 16, 255, true, 255, },
	{ 128, 256, 348, 8, 255, 255, true, 255, },
	{ 0, 256, 64, 8, 16, 255, true, 255, },
};
static TImcSongEnvelopeCounter IMCWIN_EnvCounterList[] = {
	{ 0, 0, 256 }, { -1, -1, 256 }, { 1, 0, 256 }, { 2, 0, 256 }, { 3, 0, 256 },
};
static const TImcSongOscillator IMCWIN_OscillatorList[] = {
	{ 9, 0, IMCSONGOSCTYPE_SINE, 0, -1, 64, 1, 1 },
	{ 10, 0, IMCSONGOSCTYPE_SINE, 0, -1, 176, 1, 1 },
	{ 11, 0, IMCSONGOSCTYPE_SINE, 0, -1, 116, 1, 1 },
	{ 9, 0, IMCSONGOSCTYPE_SINE, 0, -1, 255, 1, 1 },
	{ 10, 0, IMCSONGOSCTYPE_SINE, 0, 0, 255, 2, 1 },
	{ 11, 0, IMCSONGOSCTYPE_SINE, 0, 2, 54, 1, 1 },
	{ 8, 0, IMCSONGOSCTYPE_SINE, 1, -1, 100, 0, 0 },
	{ 8, 0, IMCSONGOSCTYPE_SINE, 2, -1, 100, 0, 0 },
	{ 8, 0, IMCSONGOSCTYPE_SINE, 3, -1, 100, 0, 0 },
	{ 8, 0, IMCSONGOSCTYPE_SINE, 4, -1, 100, 0, 0 },
	{ 8, 0, IMCSONGOSCTYPE_SINE, 5, -1, 100, 0, 0 },
	{ 8, 0, IMCSONGOSCTYPE_SINE, 6, -1, 100, 0, 0 },
	{ 8, 0, IMCSONGOSCTYPE_SINE, 7, -1, 100, 0, 0 },
};
static const TImcSongEffect IMCWIN_EffectList[] = {
	{ 8382, 449, 1, 0, IMCSONGEFFECTTYPE_OVERDRIVE, 0, 3 },
	{ 255, 179, 1, 0, IMCSONGEFFECTTYPE_RESONANCE, 4, 1 },
};
static unsigned char IMCWIN_ChannelVol[8] = { 158, 100, 100, 100, 100, 100, 100, 100 };
static const unsigned char IMCWIN_ChannelEnvCounter[8] = { 0, 0, 0, 0, 0, 0, 0, 0 };
static const bool IMCWIN_ChannelStopNote[8] = { false, false, false, false, false, false, false, false };
TImcSongData imcDataIMCWIN = {
	/*LEN*/ 0x1, /*ROWLENSAMPLES*/ 5512, /*ENVLISTSIZE*/ 4, /*ENVCOUNTERLISTSIZE*/ 5, /*OSCLISTSIZE*/ 13, /*EFFECTLISTSIZE*/ 2, /*VOL*/ 50,
	IMCWIN_OrderTable, IMCWIN_PatternData, IMCWIN_PatternLookupTable, IMCWIN_EnvList, IMCWIN_EnvCounterList, IMCWIN_OscillatorList, IMCWIN_EffectList,
	IMCWIN_ChannelVol, IMCWIN_ChannelEnvCounter, IMCWIN_ChannelStopNote };
#endif
