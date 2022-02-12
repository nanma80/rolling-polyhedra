#!/usr/bin/env wolframscript

exportToPov = True;
highlightCenter = False;
topView = False;
stellateFaces = False;

imageSize = {4, 3} * 720 / 3;
(* risingFrameCount = 7; *)
risingFrameCount = 75;

argv = Rest @ $ScriptCommandLine;
If[Length[argv] >= 2, 
	p = ToExpression[argv[[1]]]; q = ToExpression[argv[[2]]],
	p = 3; q = 6; (* default *)
];

maxStellationLevel = If[Length[argv] >= 3, ToExpression[argv[[3]]], 1];
maxRatio = Tan[maxStellationLevel Pi/p]/Tan[Pi/p];
nGenerations = If[Length[argv] >= 4, ToExpression[argv[[4]]], 4];

shape = "hyper_" <> ToString[p, InputForm] <> "_" <> ToString[q, InputForm];
colors = <|
	3 -> Cyan,
	5 -> Blue,
	7 -> Red
|>;

color = If[KeyExistsQ[colors, p], colors[p], Red];

splitEdgeParts = 8;

imageFolder = "output";

Needs["POVRayRender`"];
ConfigurePOVRayRender["POVRayPath" -> "C:\\Program Files\\POV-Ray\\v3.7\\bin\\pvengine64.exe"];

HInner[v_, u_, sig_] := If[sig > 0, 2 * v[[1]] * u[[1]] - Dot[v, u], Dot[v, u]];
HNormSquare[v_, sig_] := HInner[v, v, sig];
HNorm[v_, sig_] := Sqrt[HNormSquare[v, sig]];
Rotation[t_] :=
{
  {1,  0, 0},
  {0, Cos[t], Sin[t]},
  {0, -Sin[t], Cos[t]}
};
epsilon = 0.00000001;
HReflect[point_, mirror_, sig_] := If[sig!= 0, 
	point - 2 * HInner[point, mirror, sig]/HInner[mirror, mirror, sig]*mirror,
	point - 2 * (mirror[[2]]*point[[2]] + mirror[[3]]*point[[3]] + mirror[[1]])/(mirror[[2]]^2 + mirror[[3]]^2) * {0, mirror[[2]], mirror[[3]]}
	];
HDoubleReflect[point_, mirror1_, mirror2_, sig_] := HReflect[HReflect[point, mirror1, sig], mirror2, sig];
(* ApproxSamePoint[point1_, point2_] := Norm[N[point1 - point2]] < 0.00001; *)
ApproxSamePoint[point1_, point2_] := Round[point1, epsilon] == Round[point2, epsilon];

getEdgesFromFace[face_] := Table[{face[[i+1]], face[[Mod[i+1, Length[face]] + 1]]}, {i, 0, Length[face] - 1}];
sameCenter[set1_, set2_] := ApproxSamePoint[Total[set1], Total[set2]];
(* sameEdge[e1_, e2_] := sameCenter[e1, e2] && ((ApproxSamePoint[e1[[1]], e2[[1]]] && ApproxSamePoint[e1[[2]], e2[[2]]] ) || (ApproxSamePoint[e1[[1]], e2[[2]]] && ApproxSamePoint[e1[[2]], e2[[1]]] )); *)
sameEdge[e1_, e2_] := sameCenter[e1, e2];

getKlein[v_] := {0, v[[2]], v[[3]]}/v[[1]];
getPoincare[v_] := {0, v[[2]], v[[3]]}/(1+v[[1]]);
splitEdge[edge_, n_] := Table[{k edge[[1]] + (1-k) edge[[2]], (k+1/n) edge[[1]] + (1-k-1/n) edge[[2]] }, {k, 0, 1 - 1/n, 1/n}];
HNormalize[v_, norm_] := v/HNorm[v] * norm;
getHyperboloid[v_] := {-v[[1]], v[[2]], v[[3]]};

scale[edge_, ratio_] := Module[
	{center},
	center = Mean[edge];
	Map[(# - center) * ratio + center&, edge]
];

scaleFace[face_, ratio_, stellationLevel_] := Module[
	{edges, edgeOrders},
	edges = Map[scale[#, ratio]&, getEdgesFromFace[face]];
	edgeOrders = Table[Mod[stellationLevel * k, p]+1, {k, 0, p-1}];
	(* edges[[edgeOrders]] *)
	(* Map[#[[1]]&, edges[[edgeOrders]]] *)
	Flatten[edges[[edgeOrders]], 1]
];

shapeWithParams = If[highlightCenter, "highlightCenter_", ""] <> If[topView, "topView_", ""] <> If[stellateFaces, "stellateFaces_", ""] <> shape <> "_" <> IntegerString[nGenerations];

getGenerator[p_, q_] := Module[
	{inner01, inner02, inner12, signature, p0, p1, p2, otherp2, ch2psi},

	(* chi: p0 and p2 *)
	inner02 = Cot[Pi/p]*Cot[Pi/q];
	(* psi: p1 and p2 *)
	inner12 = Cos[Pi/q]/Sin[Pi/p];
	(* phi: p0 and p1 *)
	inner01 = Cos[Pi/p]/Sin[Pi/q];

	signature = Sign[1/2-1/p-1/q];
	(* signature = 1 for hyperbolic, -1 for spherical, 0 for planar *)

	p0 = {inner02, 
		Sqrt[signature*(inner02^2-1)], 
		0}//Simplify;
	p1 = {inner12, 
		Sqrt[signature*(inner12^2-1)] Cos[Pi/p], 
		Sqrt[signature*(inner12^2-1)] Sin[Pi/p]}//Simplify;
	(* other p2 such that the midpoint of p2 and otherp2 is p1 *)
	p2 = {1, 0, 0};

	ch2psi = 2 * inner12 ^ 2 - 1;
	Print["ch2psi"];
	Print[ch2psi//N];
	(* is 1 for {3,6} *)
	otherp2 = {ch2psi, 
		Sqrt[signature*(ch2psi^2-1)] Cos[Pi/p], 
		Sqrt[signature*(ch2psi^2-1)] Sin[Pi/p]};
	

	mirror1 = {0, 0, 1};
	mirror2 = {0, -Sin[Pi/p], Cos[Pi/p]};
	mirror3 = otherp2 - p2//Simplify;
	If[signature == 0,
		p0 = {1, 1, 0};
		p1 = {1, Cos[Pi/p], Sin[Pi/p]}*Cos[Pi/p];
		p2 = {1, 0, 0};
		otherp2 = 2 p1 - p2;
		mirror3 = {-Cos[Pi/p], Cos[Pi/p], Sin[Pi/p]};
	];

	vertex1 = p0;

	Print[p0//N];
	Print[p1//N];
	Print[otherp2//N];
	Print["dihedral angle"];
	Print[ArcCos[Dot[p2, otherp2]/Norm[p2]/Norm[otherp2]]/Pi * 180. ];

	mirrors = {mirror1, mirror2, mirror3}//N;
	Print["mirrors"];
	Print[mirrors//N];

	{vertex1, {mirror1, mirror2, mirror3}//N, signature}
];

generateFaces[p_, q_] := Module[
	{},

	generator = getGenerator[p, q];
	vertex1 = generator[[1]];
	mirrors = generator[[2]];
	signature = generator[[3]];

	face1 = {vertex1}//N;
	finished = False;
	For[i = 0, i < 20 && finished == False, i++,
		newVertex = FullSimplify[HDoubleReflect[face1[[-1]], mirrors[[1]], mirrors[[2]], signature]];
		If[!(ApproxSamePoint[newVertex, face1[[1]]]),
			AppendTo[face1, newVertex],
			finished = True;
		]
	];

	Print["Each face contains " <> IntegerString[Length[face1]] <> " vertices."];

	nGenerations = 7;
	If[signature > 0, nGenerations = 6];
	If[signature < 0, nGenerations = 4];

	faces = {face1};
	countStart = -1;
	For[i = 0, i <= nGenerations && countStart != Length[faces], i++,
		Print["i = " <> IntegerString[i]];
		countStart = Length[faces];
		For[mirrorIndex = 3, mirrorIndex >= 1, mirrorIndex--,
			newFaces = Map[HReflect[#, mirrors[[mirrorIndex]] , signature ]&, faces, {2}];
			faces = Union[faces, newFaces, SameTest -> sameCenter];
		];
	];

	Print["Making it more symmetric"];

	(* Making it symmetric *)
	centeredMirrors = Select[mirrors, #[[1]] == 0 &];

	countStart = -1;
	For[i = 0, i < 20 && countStart != Length[faces], i++,
		Print["i = " <> IntegerString[i]];
		countStart = Length[faces];
		For[mirrorIndex = Length[centeredMirrors], mirrorIndex >= 1, mirrorIndex--,
			newFaces = Map[HReflect[#, centeredMirrors[[mirrorIndex]], signature ]&, faces, {2}];
			faces = Union[faces, newFaces, SameTest -> sameCenter];
		];
	];

	scaledFaces = Map[(# / vertex1[[2]] + {1- vertex1[[1]]/vertex1[[2]], 0, 0} )&, faces, {2}];
	scaledFaces
]

elements3D[faces_, stellationLevel_, spinAngle_, sig_] := Module[
	{allEdges, edges, allVertices, vertices, ratio, selectedFaces, displayFaces},

	ratio = Tan[stellationLevel Pi/p]/Tan[Pi/p]//Simplify;
	Print[{stellationLevel, ratio}];

	rotation = RotationMatrix[spinAngle, {1, 0, 0}];
	(* selectedFaces = Select[faces, (Total[#]/Length[#])[[1]] < 10 &]; *)
	selectedFaces = Map[(#.rotation) &, faces, {2}];
	(* kleinFaces = Map[getKlein, faces, {2}]; *)
	displayFaces = Map[getHyperboloid, selectedFaces, {2}];

	If[stellateFaces, 
		displayFaces = Map[scaleFace[#, ratio, Ceiling[stellationLevel]]&, displayFaces]
	];

	(* Print["There are " <> IntegerString[Length[selectedFaces]] <> " faces."]; *)
	allEdges = Flatten[Map[getEdgesFromFace, selectedFaces], 1];
	edges = Union[allEdges , SameTest -> sameEdge];

	(* Print["There are " <> IntegerString[Length[edges]] <> " edges in total."]; *)
	(* norm = HNorm[edges[[1]][[1]]]; *)

	(* splitEdges = Flatten[ Map[splitEdge[#, splitEdgeParts]&, edges], 1]; *)
	(* splitEdges = Map[HNormalize[#, norm]&, splitEdges, {2}]; *)
	(* Print["There are " <> IntegerString[Length[splitEdges]] <> " edges after split."]; *)
	displayEdges = Map[getHyperboloid, edges, {2}];
	displayEdges = Map[scale[#, ratio] &, displayEdges];

	allVertices = Round[Flatten[displayEdges, 1], epsilon];
	vertices = Union[allVertices , SameTest -> ApproxSamePoint];
	verticesTally = Tally[allVertices];
	verticesTallyCounts = Map[#[[2]]&, verticesTally];
	tallyTally = Tally[verticesTallyCounts];
	(* Print["There are " <> IntegerString[Length[vertices]] <> " vertices in total."]; *)
	Print[Map[Length, {faces, allEdges, edges, allVertices, vertices}]];
	Print[tallyTally];

	centeredFaces = Select[faces, (Abs[Total[#][[2]]]<0.001)&&(Abs[Total[#][[3]]]<0.001)&];
	displayCenteredFaces = Map[getHyperboloid, centeredFaces, {2}];
	displayCenteredFaces = Map[(# + {0.001, 0, 0})&, displayCenteredFaces, {2}];

	centeredEdges = Flatten[Map[getEdgesFromFace, centeredFaces], 1];
	

	(* splitCenteredEdges = Map[HNormalize[#, norm]&, splitCenteredEdges, {2}]; *)
	(* Print["There are " <> IntegerString[Length[centeredEdges]] <> " centered edges after split."]; *)
	displayCenteredEdges = Map[getHyperboloid, centeredEdges, {2}];

	(* centeredVertices = displayCenteredFaces[[1]]; *)

	(* planar *)
	color = Blue;
	opacity = 0.5;

	(* hyperbolic *)
	If[sig > 0, color = Red; opacity = 1];

	(* spherical, default to 0.4*)
	If[sig < 0, color = Green; opacity = 0.0];

	elements = {
			{
				Opacity[opacity], FaceForm[color], Map[Polygon, displayFaces]
			},
			{
				Opacity[1], FaceForm[White], Map[Cylinder[#, thickness]&, displayEdges]
			},
			{
				Opacity[1], FaceForm[White], Map[Sphere[#[[1]], thickness * rangeZoom]&, verticesTally]
			}
	};

	If[highlightCenter,
		highlightColor = White;
		elements = Union[
			elements,
			{
				{
					Opacity[1], FaceForm[highlightColor], Map[Polygon, displayCenteredFaces]
				},
				{
					Opacity[1], FaceForm[highlightColor], Map[Cylinder[#, thickness * 2.2]&, displayCenteredEdges]
				}
			}
		];
	];
	elements
];

exportFrame[stellationLevel_, spinAngle_, frameIndex_, outputFolderName_] := Module[
	{},

	elements35 = elements3D[faces35, stellationLevel, spinAngle, -1];
	elements36 = elements3D[faces36, stellationLevel, spinAngle, 0];
	elements37 = elements3D[faces37, stellationLevel, spinAngle, 1];
	elements = Union[elements35, elements36, elements37];

	rangeZoom = Sqrt[maxRatio] * 1.2 * If[p < 6, 1, 0.5];
	lighting = {{"Point", White, {10, -10, 10}}};
	range = If[topView, 2, 3.5] * rangeZoom;
	thickness = range / 80.0 / rangeZoom * If[p < 6, 1, 0.7];
	viewCenter = {-3.5, 0, 0};

	graph = Graphics3D[
		elements,
		ViewPoint -> If[topView, {10, 0, 0}, {3.5, -2.5, 3}],
		ViewVertical -> If[topView, {0, 1, 0}, {1, 0, 0}],
		PlotRange -> {{viewCenter[[1]]-range, viewCenter[[1]]+range}, {viewCenter[[2]]-range, viewCenter[[2]]+range}, {viewCenter[[3]]-range, viewCenter[[3]]+range}},
		Lighting -> lighting
	];

	outputFolder = FileNameJoin[{imageFolder, outputFolderName}];
	If[!DirectoryQ[outputFolder], CreateDirectory[outputFolder]];

	outputFileName = "spinning_frame_" <> IntegerString[frameIndex, 10, 4] <> ".png";
	outputFileName = FileNameJoin[{outputFolder, outputFileName}];

	cropCommand = "mogrify -crop " <> IntegerString[imageSize[[2]]] <> "x" <> IntegerString[imageSize[[2]]] <> "+" <> IntegerString[(imageSize[[1]]-imageSize[[2]])/2] <> "+0 " <> outputFileName;

	If[exportToPov,
	  If[FileExistsQ[outputFileName], DeleteFile[outputFileName]];
	  POVRayRender[graph, Method -> "Triangle", OutputPath -> outputFileName, ImageSize -> imageSize, RenderOptions -> "+A0.001 -J +UA"];
	  Run[cropCommand];
	  ,
	  Export[outputFileName, graph];
	];
	Print["Saved an image to " <> outputFileName];

];

(* exportAnimation[] := Module[
	{
		outputFolder,
		gifFileName,
		gifFolder,
		maxExplosionFactor,
		explodeStep,
		explosionFactors,
		frameIndex,
		explosionFactor,
		gifCommand
	},


	outputFolderName = shapeWithParams <> "_spinning_frames";
	outputFolder = FileNameJoin[{imageFolder, outputFolderName}];
	If[!DirectoryQ[outputFolder], CreateDirectory[outputFolder]];

	gifFileName = shapeWithParams <> "_spinning";
	gifFolder = FileNameJoin[{imageFolder, shape}];
	If[!DirectoryQ[gifFolder], CreateDirectory[gifFolder]];

	stellationLevels = {};
	stayingStellationLevels = Table[1, {frameIndex, 1, Ceiling[risingFrameCount/8], 1}];
	stellationLevels = Join[stellationLevels, stayingStellationLevels];

	For[level = 2, level <= maxStellationLevel, level ++,
		stayingStellationLevels = Table[level-1, {frameIndex, 1, Ceiling[risingFrameCount/8], 1}];
		stellationLevels = Join[stellationLevels, stayingStellationLevels];
		increasingStellationLevels = Table[level - 1 + (1 - Cos[(frameIndex - 1)/(risingFrameCount - 1) * Pi 1.0])/2, {frameIndex, 1, risingFrameCount}];
		stellationLevels = Join[stellationLevels, increasingStellationLevels];
		stayingStellationLevels = Table[level, {frameIndex, 1, Ceiling[risingFrameCount/8], 1}];
		stellationLevels = Join[stellationLevels, stayingStellationLevels];
	];
	stayingStellationLevels = Table[maxStellationLevel, {frameIndex, 1, Ceiling[risingFrameCount/8], 1}];
	stellationLevels = Join[stellationLevels, stayingStellationLevels];
	stellationLevels = Join[stellationLevels, Reverse[stellationLevels]];

	frameCount = Length[stellationLevels];

	For[frameIndex = 1, frameIndex <= frameCount , frameIndex++,
		stellationLevel = stellationLevels[[frameIndex]];
		spinAngle = frameIndex/frameCount * 2.0 Pi / p;
		Print["Frame #" <> IntegerString[frameIndex] <> "/" <> IntegerString[frameCount] <> " " <> ToString[stellationLevel] <> " " <> ToString[spinAngle] <> " " <> outputFolderName];

		exportFrame[stellationLevel, spinAngle, frameIndex, outputFolderName];
	];
	
	delay = If[frameCount >= 50, 3, Floor[500/frameCount]];

	gifCommand = "magick convert -quiet +repage -alpha remove -loop 0 -delay " <> IntegerString[delay] <> " -dispose 2 " <> outputFolder <> "\\spinning_frame*.png " <> gifFolder <> "\\" <> gifFileName <> ".gif";

	Print[gifCommand];
	Run[gifCommand];
	Print["Saved the animation to " <> gifFolder <> "\\" <> gifFileName <> ".gif"];
	
  mp4Command = "ffmpeg -i " <> gifFolder <> "\\" <> gifFileName <> ".gif" 
      <> " -movflags faststart -pix_fmt yuv420p -vf \"scale=trunc(iw/2)*2:trunc(ih/2)*2\" "
      <> gifFolder <> "\\" <> gifFileName <> ".mp4";

  mp4FileName = gifFolder <> "\\" <> gifFileName <> ".mp4";
  If[FileExistsQ[mp4FileName], DeleteFile[mp4FileName]];
  Print[mp4Command];
  Run[mp4Command];
  Print["Saved the video to " <> gifFolder <> "\\" <> gifFileName <> ".mp4"];
];
 *)

Print[shape];
(* faces = generateFaces[p, q]; *)
faces35 = generateFaces[p, q-1];
faces36 = generateFaces[p, q];
faces37 = generateFaces[p, q+1];

Print[Map[Length, {faces35, faces36, faces37}]];

int56 = Intersection[faces35, faces36, SameTest -> sameCenter];
int67 = Intersection[faces37, faces36, SameTest -> sameCenter];
intAll = Intersection[int56, int67, SameTest -> sameCenter];
Print[int56//Round[#, epsilon]&];
Print[int67//Round[#, epsilon]&];
Print[intAll//Round[#, epsilon]&];

edges = getEdgesFromFace[intAll[[1]]];
Print[edges//Round[#, epsilon]&];

rotateEdge = edges[[1]];
edgeCenter = Mean[rotateEdge];
edgeVector = rotateEdge[[1]] - rotateEdge[[2]];
Print[edgeVector//N];

rotated[vertices_, center_, axis_, angle_] := Map[# + center &, Map[# - center &, vertices].RotationMatrix[angle, axis]];

(* boost[t_] :=
{
  {Cosh[t], Sinh[t], 0},
  {Sinh[t], Cosh[t], 0},
  {0,       0,       1}
};
 *)

boost[t_] :=
{
  {Cosh[t], 0, Sinh[t]},
  {0, 1, 0},
  {Sinh[t], 0, Cosh[t]}
};

(* only works if axis[[1]] == 0 *)
boostMatrix[angle_, axis_] := RotationMatrix[Arg[axis[[2]] + I axis[[3]]], {1, 0, 0}].boost[angle].RotationMatrix[-Arg[axis[[2]] + I axis[[3]]], {1, 0, 0}];

boosted[vertices_, center_, axis_, angle_] := Map[# + center &, Map[# - center &, vertices].boostMatrix[angle, axis]];


dihedral35 = (Pi-ArcCos[-Sqrt[5]/3]);
Print["rotate"];

exportFrame[1.0, 0, 10, "test"];

faces37 = Map[boosted[#, edgeCenter, edgeVector, 0.1]&, faces37];
exportFrame[1.0, 0, 11, "test"];


(* faces35 = Map[rotated[#, edgeCenter, edgeVector, dihedral35]&, faces35]; *)
(* exportFrame[1.0, 0, 3, "test"]; *)

(* int56 = Intersection[faces35, faces36, SameTest -> sameCenter];
int67 = Intersection[faces37, faces36, SameTest -> sameCenter];
intAll = Intersection[int56, int67, SameTest -> sameCenter];
Print[int56//Round[#, epsilon]&];
Print[int67//Round[#, epsilon]&];
Print[intAll//Round[#, epsilon]&];
 *)

(* exportAnimation[]; *)

(* exportFrame[1.0, 0, 0, "test"]; *)

(* faces35 = Map[rotated[#, edgeCenter, edgeVector, dihedral35/4]&, faces35];
exportFrame[1.0, 0, 1, "test"];
faces35 = Map[rotated[#, edgeCenter, edgeVector, dihedral35/4]&, faces35];
exportFrame[1.0, 0, 2, "test"];
faces35 = Map[rotated[#, edgeCenter, edgeVector, dihedral35/4]&, faces35];
exportFrame[1.0, 0, 3, "test"];
faces35 = Map[rotated[#, edgeCenter, edgeVector, dihedral35/4]&, faces35];
exportFrame[1.0, 0, 4, "test"];
 *)

(* exportFrame[1.0, 0, 2, "test"]; *)
(* exportFrame[1.2, 0, 2, "test"]; *)
(* exportFrame[1.4, 0, 3, "test"]; *)
(* exportFrame[1.6, 0, 4, "test"]; *)
(* exportFrame[1.8, 0, 5, "test"]; *)
(* exportFrame[2.0, 0, 6, "test"]; *)
(* exportFrame[2.2, 0, 7, "test"]; *)
(* exportFrame[2.4, 0, 8, "test"]; *)
(* exportFrame[2.6, 0, 9, "test"]; *)
(* exportFrame[2.8, 0, 10, "test"]; *)
(* exportFrame[3.0, 0, 11, "test"]; *)

