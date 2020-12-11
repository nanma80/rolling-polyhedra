#!/usr/bin/env wolframscript

imageFolder = "output";
imageSize = 240 * {4, 3};
exportToPov = False;
exportToPov = True;
epsilon = 0.000000001;
stepCount = 36;
framesPerMove = If[outer, 30, 18];

outer = True;
(* outer = False; *)

(* traceEdges = True; *)
traceEdges = False;

(* traceNodes = True; *)
traceNodes = False;

showFaces = False;

If[Length[$ScriptCommandLine] <= 1,
    Print["Pass config as command line parameters. For example:"];
    Print["wolframscript.exe dodeca_great.wls inner edges nodes faces"],
    If[$ScriptCommandLine[[2]] == "outer" || $ScriptCommandLine[[2]] == "inner",
        outer = ($ScriptCommandLine[[2]] == "outer"),
        Print["First parameter must be either inner or outer"];
    ];
    If[Length[$ScriptCommandLine] >= 3,
        otherParams = Table[$ScriptCommandLine[[k]], {k, 3, Length[$ScriptCommandLine]}];
        traceEdges = ContainsAny[otherParams, {"edges", "edge"}];
        traceNodes = ContainsAny[otherParams, {"nodes", "node"}];
        showFaces = ContainsAny[otherParams, {"faces", "face"}];
    ];
];

Print["outer, traceEdges, traceNodes, showFaces, stepCount:"];
Print[{outer, traceEdges, traceNodes, showFaces, stepCount}];


stepCountNoChange = stepCount / 5;
(* need 12 vertex colors and 30 edge colors *)
basicColors = {Red, Yellow, Blue, Green, Cyan, Magenta};
vertexColors = Union[basicColors, Map[Lighter[#]&, basicColors]];
edgeColors = Union[basicColors, Map[Lighter[#, 0.3]&, basicColors], Map[Lighter[#, 0.7]&, basicColors], Map[Darker[#, 0.3]&, basicColors], Map[Darker[#, 0.7]&, basicColors]];

outputFolder = FileNameJoin[{imageFolder, "dodeca_great_animation_frames_" <> If[outer, "outer", "inner"] <> "_" <> If[traceEdges, "with_edges", "no_edges"] <> "_" <> If[traceNodes, "with_nodes", "no_nodes"] <> If[showFaces, "_with_faces", ""] }];
If[!DirectoryQ[outputFolder], CreateDirectory[outputFolder]];

Needs["POVRayRender`"];
ConfigurePOVRayRender["POVRayPath" -> "C:\\Program Files\\POV-Ray\\v3.7\\bin\\pvengine64.exe"];

cubeVertices[dimension_] := 2 * Map[IntegerDigits[#, 2][[2;;dimension+1]]&, Table[k, {k, 2^dimension, 2 * 2^dimension - 1}]] - 1;
allPlusMinus[v_] := Union[Map[(v #)&, cubeVertices[Length[v]]]];
rotated[vertices_, center_, axis_, angle_] := Map[# + center &, Map[# - center &, vertices].RotationMatrix[angle, axis]];
getEdgesFromFace[face_] := Table[{face[[i+1]], face[[Mod[i+1, Length[face]] + 1]]}, {i, 0, Length[face] - 1}];

getTriangularFaces[vertices_] := Module[
    {nvertices, edgeLength, i, j, k},
    faces = {};
    edgeLength = 10^10;
    nvertices = N[vertices];
    For[i = 2, i < Length[vertices], i++, 
        If[ N[Norm[nvertices[[i]] - nvertices[[1]]]] < edgeLength, edgeLength = N[Norm[nvertices[[i]] - nvertices[[1]]]]];
    ];

    For[i = 1, i <= Length[vertices]-2, i++, 
        For[j = i + 1, j <= Length[vertices]-1, j++, 
            If[Abs[N[Norm[nvertices[[i]] - nvertices[[j]]]] - edgeLength] < epsilon,
                For[k = j + 1, k <= Length[vertices], k++,
                    If[Abs[N[Norm[nvertices[[i]] - nvertices[[k]]]] - edgeLength] < epsilon 
                        && Abs[N[Norm[nvertices[[k]] - nvertices[[j]]]] - edgeLength] < epsilon,
                        AppendTo[faces, {i, j, k}];
                    ];
                ];   
            ];
        ];
    ];
    faces
];



viewPoint = {3, -2.5, 2} * 10;
vertical = {0, 1, phi};
cummulatedVerticesGreatDodeca = Table[{}, {k, 12}];
cummulatedEdgesTetra = Table[{}, {k, 30}];
viewRotationAngle = -Pi/4.;

drawImage[verticesDodeca_, edgeIndicesDodeca_, rotatedVerticesGreatDodeca_, edgeIndicesGreatDodeca_, serialNumber_] := Module[
    {thickness, range, lighting, graph, outputFileName, cropCommand, nodeIndex, edgeIndex},
    thickness = 0.035 * If[outer, 1, 1/2];
    range = If[outer, 2.7, 1.5];
    lighting = {{"Point", White, {10, -10, 10}}};
    modifyingGraphElements = Length[rotatedVerticesGreatDodeca] > 0;

    If[modifyingGraphElements,
        If[traceNodes,
            For[nodeIndex = 1, nodeIndex <= Length[rotatedVerticesGreatDodeca], nodeIndex++,
                AppendTo[cummulatedVerticesGreatDodeca[[nodeIndex]], rotatedVerticesGreatDodeca[[nodeIndex]]]
            ]
        ];

        If[traceEdges,
            For[edgeIndex = 1, edgeIndex <= Length[edgeIndicesGreatDodeca], edgeIndex++,
                AppendTo[cummulatedEdgesTetra[[edgeIndex]], rotatedVerticesGreatDodeca[[edgeIndicesGreatDodeca[[edgeIndex]]]]]
            ]
        ];
    ];

    angleChange = 2 * Pi/stepCount/(framesPerMove + 1);
    If[angleChange > Pi/32, angleChange = Pi/32];
    viewRotationAngle = viewRotationAngle + angleChange;
    rotationMatrix = RotationMatrix[viewRotationAngle, vertical];

    graphElements = {};

    (* dodeca *)
    graphElements = Join[graphElements, Map[{Sphere[#.rotationMatrix, 1.5 * thickness], thickness}&, verticesDodeca]];
    graphElements = Join[graphElements, Map[{Cylinder[verticesDodeca[[#]].rotationMatrix, thickness * 0.75]}&, edgeIndicesDodeca]];

    (* nodes *)
    If[Length[cummulatedVerticesGreatDodeca[[1]]] > 0,
        graphElements = Join[graphElements, Map[{FaceForm[vertexColors[[ Mod[#, Length[vertexColors]] + 1 ]]], Sphere[cummulatedVerticesGreatDodeca[[#]].rotationMatrix, 3 * thickness * (1 + 0.01 * #)], thickness}&, Range[Length[cummulatedVerticesGreatDodeca]]]];
    ];

    If[!traceNodes && modifyingGraphElements,
        graphElements = Join[graphElements, Map[{FaceForm[vertexColors[[Mod[#, Length[vertexColors]] + 1 ]]], Sphere[rotatedVerticesGreatDodeca[[#]].rotationMatrix, 3 * thickness * (1 + 0.01 * #)], thickness}&, Range[Length[rotatedVerticesGreatDodeca]]]];
    ];

    (* edges *)
    If[Length[cummulatedEdgesTetra[[1]]] > 0,
        graphElements = Join[graphElements,
            Map[{FaceForm[edgeColors[[ Mod[#, Length[edgeColors]] + 1 ]]], Cylinder[cummulatedEdgesTetra[[#]].rotationMatrix, thickness * 0.76 * (1 + 0.01 * #)]}&, Range[Length[cummulatedEdgesTetra]]]
        ];
    ];

    If[!traceEdges && modifyingGraphElements,
        graphElements = Join[graphElements,
            Map[{FaceForm[edgeColors[[ Mod[#, Length[edgeColors]] + 1 ]]], Cylinder[rotatedVerticesGreatDodeca[[edgeIndicesGreatDodeca[[#]]]].rotationMatrix, thickness * 0.76 * (1 + 0.01 * #)]}&, Range[Length[edgeIndicesGreatDodeca]]]
        ];
    ];

    (* show rotated great dodec regardless of setting *)
    If[modifyingGraphElements,
        graphElements = Join[graphElements,
            Map[{Opacity[1], FaceForm[Lighter[Blue, 0.1]], Polygon[rotatedVerticesGreatDodeca[[faceIndicesGreatDodeca[[#]]]].rotationMatrix]}&, Range[Length[faceIndicesGreatDodeca]]]
        ];
    ];

    If[showFaces,
        graphElements = Join[graphElements,
            Map[{Opacity[If[outer, 0.5, 0.25]], FaceForm[Yellow], Polygon[verticesDodeca[[faceIndicesDodeca[[#]]]].rotationMatrix * If[outer, 0.9999, 1.0001]]}&, Range[Length[faceIndicesDodeca]]]
        ];
    ];

    graph = Graphics3D[
        graphElements,
        ViewPoint -> viewPoint,
        ViewVertical -> vertical,
        PlotRange -> {{-range, range}, {-range, range}, {-range, range}},
        Lighting -> lighting
    ];

    outputFileName = "frame_" <> IntegerString[serialNumber, 10, 4] <> ".png";
    outputFileName = FileNameJoin[{outputFolder, outputFileName}];

    cropCommand = "mogrify -crop " <> IntegerString[imageSize[[2]]] <> "x" <> IntegerString[imageSize[[2]]] <> "+" <> IntegerString[(imageSize[[1]]-imageSize[[2]])/2] <> "+0 " <> outputFileName;

    If[exportToPov,
        If[FileExistsQ[outputFileName], DeleteFile[outputFileName]];
        POVRayRender[graph, 
            Method -> "Triangle", 
            OutputPath -> outputFileName, 
            ImageSize -> imageSize
            , RenderOptions -> "+A0.001 -J +UA"
        ];
        Run[cropCommand];
        ,
        Export[outputFileName, graph];
    ];
];



exportAnimation[] := Module[{},
    gifFileName = "dodeca_great_" <> If[outer, "outer", "inner"] <> "_" <> If[traceEdges, "with_edges", "no_edges"] <> "_" <> If[traceNodes, "with_nodes", "no_nodes"] <> If[showFaces, "_with_faces", ""];
    frameCount = stepCount * (1 + framesPerMove);
    gifFolder = imageFolder;
    gifCommand = "magick convert -quiet +repage -alpha remove -loop 0 -delay " <> IntegerString[Floor[100/framesPerMove]] <> " -dispose 2 " <> outputFolder <> "\\*.png " <> gifFolder <> "\\" <> gifFileName <> ".gif";

    mp4Command = "ffmpeg -i " <> gifFolder <> "\\" <> gifFileName <> ".gif" 
        <> " -movflags faststart -pix_fmt yuv420p "
        <> gifFolder <> "\\" <> gifFileName <> ".mp4";

    Print[gifCommand];
    Run[gifCommand];
    Print["Saved the animation to " <> gifFolder <> "\\" <> gifFileName <> ".gif"];

    mp4FileName = gifFolder <> "\\" <> gifFileName <> ".mp4";
    If[FileExistsQ[mp4FileName], DeleteFile[mp4FileName]];
    Print[mp4Command];
    Run[mp4Command];
    Print["Saved the video to " <> gifFolder <> "\\" <> gifFileName <> ".mp4"]
];



getPentagonFaces[vertices_] := Module[
    {edgeLength, epsilon, i, j, k, l, m, phi},
    phi = GoldenRatio;
    faces = {};
    edgeLength = 10^10;
    epsilon = 0.0001;
    For[i = 2, i <= Length[vertices], i++, 
        If[ Norm[vertices[[i]] - vertices[[1]]] < edgeLength, edgeLength = Norm[vertices[[i]] - vertices[[1]]]];
    ];
    Print["Pentagonal face edge length:"];
    Print[edgeLength];
    For[i = 1, i <= Length[vertices]-1, i++, 
        For[j = i + 1, j <= Length[vertices], j++, 
            If[Abs[ Norm[vertices[[i]] - vertices[[j]]] - edgeLength] < epsilon,
                For[k = i + 1, k <= Length[vertices], k++,
                    If[ Abs[ Norm[vertices[[j]] - vertices[[k]]] - edgeLength] < epsilon &&
                        Abs[ Norm[vertices[[i]] - vertices[[k]]] - phi * edgeLength] < epsilon,
                        If[Det[{vertices[[i]] - vertices[[j]], vertices[[j]] - vertices[[k]], vertices[[i]] + vertices[[j]] + vertices[[k]]}] < 0, Continue[]];
                        For[l = i + 1, l <= Length[vertices], l++,
                            If[ Abs[ Norm[vertices[[k]] - vertices[[l]]] - edgeLength] < epsilon &&
                                Abs[ Norm[vertices[[l]] - vertices[[j]]] - phi * edgeLength] < epsilon,
                                For[m = i + 1, m <= Length[vertices], m++,
                                    If[
                                        Abs[ Norm[vertices[[l]] - vertices[[m]]] - edgeLength] < epsilon &&
                                        Abs[ Norm[vertices[[i]] - vertices[[m]]] - edgeLength] < epsilon &&
                                        Abs[ Norm[vertices[[j]] - vertices[[m]]] - phi * edgeLength] < epsilon &&
                                        Abs[ Norm[vertices[[k]] - vertices[[m]]] - phi * edgeLength] < epsilon,

                                        AppendTo[faces, {i, j, k, l, m}]

                                    ];
                                ];
                            ];
                        ];
                    ];
                ];   
            ];
        ];
    ];
    faces
];


(* initialization *)
phi = (Sqrt[5] + 1) / 2;
verticesSeedsDodeca = {
    {1, 1, 1},
    {0, phi, 1/phi},
    {phi, 1/phi, 0},
    {1/phi, 0, phi}
};

verticesDodeca = Union[Flatten[Map[allPlusMinus, verticesSeedsDodeca], 1]];
faceIndicesDodeca = getPentagonFaces[verticesDodeca * 1.0];

(* 
{{1, 5, 6, 2, 13}, {1, 13, 14, 3, 15}, {1, 15, 17, 9, 5}, {2, 6, 10, 18, 16},
{2, 16, 4, 14, 13}, {3, 7, 11, 17, 15}, {3, 14, 4, 8, 7}, {4, 16, 18, 12, 8},
{5, 9, 19, 10, 6}, {7, 8, 12, 20, 11}, {9, 17, 11, 20, 19}, {10, 19, 20, 12, 18}}

 *)
edgeLength = Norm[verticesDodeca[[faceIndicesDodeca[[1]][[1]]]] - verticesDodeca[[faceIndicesDodeca[[1]][[2]]]]]//Simplify;
edgeIndicesDodeca = Union[Map[Sort, Flatten[Map[getEdgesFromFace, faceIndicesDodeca], 1]]];

faceIndex = 1;
faceIndices1 = faceIndicesDodeca[[faceIndex]];
edgeIndices1 = faceIndices1[[{1, 2}]];
neighboringFaces = Select[faceIndicesDodeca, ContainsAll[#, edgeIndices1]&];
face1 = verticesDodeca[[neighboringFaces[[1]]]];
face2 = verticesDodeca[[neighboringFaces[[2]]]];

edgeCenter = Mean[verticesDodeca[[edgeIndices1]]];
edgeVector = verticesDodeca[[edgeIndices1]][[1]] - verticesDodeca[[edgeIndices1]][[2]];
icosaRatio = Norm[{0, 1, phi}]/2;


face1Total = Total[face1];
kSolutions = Solve[Norm[k * face1Total - verticesDodeca[[faceIndicesDodeca[[1]][[1]]]]]/icosaRatio == edgeLength, {k}]//FullSimplify;
ks = Sort[k/.kSolutions, #1<#2 &];
kFactor = ks[[ If[outer, 2, 1] ]];
icosaCenter1 = face1Total * kFactor//FullSimplify;
icosaCenter2 = Total[face2] * kFactor//FullSimplify;

radial = face1Total/Norm[face1Total]*Norm[icosaCenter1-face1[[1]]]//FullSimplify;

verticesGreatDodeca = Join[{icosaCenter1 - radial}, face1, Map[(2 * icosaCenter1 - #)&, face1], {icosaCenter1 + radial}]//FullSimplify;

faceIndicesGreatDodeca = getPentagonFaces[verticesGreatDodeca * 1.0];
edgeIndicesGreatDodeca = Union[Map[Sort, Flatten[Map[getEdgesFromFace, faceIndicesGreatDodeca], 1]]];



v1 = icosaCenter1 - edgeCenter;
v2 = icosaCenter2 - edgeCenter;
theta = ArcCos[(v1.v2)/Norm[v1]/Norm[v2]]//FullSimplify;
(* outer: 180. inner: 116.56505 - 63.435=53.1301 *)


sign = Sign[Det[{edgeVector, edgeCenter, face1Total}]] * If[outer, 1, -1];
rotatedVerticesGreatDodeca = rotated[verticesGreatDodeca, edgeCenter, edgeVector, sign * theta/2];

pathFaces = {
    {1, 5, 6, 2, 13},
    {1, 15, 17, 9, 5},
    {9, 17, 11, 20, 19},
    {5, 9, 19, 10, 6},
    {10, 19, 20, 12, 18},
    {4, 16, 18, 12, 8},
    {7, 8, 12, 20, 11},
    {3, 14, 4, 8, 7},
    {2, 16, 4, 14, 13},
    {1, 13, 14, 3, 15},
    {3, 14, 4, 8, 7},
    {3, 7, 11, 17, 15},
    {9, 17, 11, 20, 19},
    {7, 8, 12, 20, 11},
    {10, 19, 20, 12, 18},
    {2, 6, 10, 18, 16},
    {2, 16, 4, 14, 13},
    {1, 5, 6, 2, 13},
    {1, 13, 14, 3, 15},
    {3, 7, 11, 17, 15},
    {1, 15, 17, 9, 5},
    {1, 5, 6, 2, 13},
    {2, 6, 10, 18, 16},
    {4, 16, 18, 12, 8},
    {3, 14, 4, 8, 7},
    {1, 13, 14, 3, 15},
    {1, 15, 17, 9, 5},
    {5, 9, 19, 10, 6},
    {10, 19, 20, 12, 18},
    {9, 17, 11, 20, 19},
    {3, 7, 11, 17, 15},
    {7, 8, 12, 20, 11},
    {4, 16, 18, 12, 8},
    {2, 16, 4, 14, 13},
    {2, 6, 10, 18, 16},
    {5, 9, 19, 10, 6},
    {1, 5, 6, 2, 13}
};


pathEdges = Table[Intersection[pathFaces[[i]], pathFaces[[i + 1]]], {i, Length[pathFaces] - 1}];

serialNumber = 1;

For[stepIndex = 1, stepIndex <= stepCount, stepIndex++,
    faceIndices1 = pathFaces[[stepIndex]];
    edgeIndices1 = pathEdges[[stepIndex]];

    Print[{stepIndex, faceIndices1, edgeIndices1}];

    edgeCenter = Mean[verticesDodeca[[edgeIndices1]]];
    edgeVector = verticesDodeca[[edgeIndices1]][[1]] - verticesDodeca[[edgeIndices1]][[2]];

    face1 = verticesDodeca[[faceIndices1]];
    face1Total = Total[face1];

    sign = Sign[Det[{edgeVector, edgeCenter, face1Total}]] * If[outer, 1, -1];

    For[frameIndex = 0, frameIndex < framesPerMove, frameIndex++,
        rotatedVerticesGreatDodeca = rotated[verticesGreatDodeca, edgeCenter, edgeVector, sign * theta / framesPerMove * frameIndex];
        drawImage[verticesDodeca, edgeIndicesDodeca, rotatedVerticesGreatDodeca, edgeIndicesGreatDodeca, serialNumber];
        serialNumber = serialNumber + 1;
    ];

    verticesGreatDodeca = rotated[verticesGreatDodeca, edgeCenter, edgeVector, sign * theta]//FullSimplify;

    drawImage[verticesDodeca, edgeIndicesDodeca, verticesGreatDodeca, edgeIndicesGreatDodeca, serialNumber];
    serialNumber = serialNumber + 1;
];

(* no change *)
If[traceNodes || traceEdges,
    For[i = 1, i <= (framesPerMove + 1) * stepCountNoChange, i++,
        Print[{"no change", i, (framesPerMove + 1) * stepCountNoChange}];
        drawImage[verticesDodeca, edgeIndicesDodeca, {}, {}, serialNumber];
        serialNumber = serialNumber + 1;
    ];
];

exportAnimation[];

