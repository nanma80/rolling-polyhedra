#!/usr/bin/env wolframscript

imageFolder = "output";
imageSize = 150 * {4, 3};
exportToPov = False;
exportToPov = True;
epsilon = 0.000000001;
outer = True;
outer = False;
stepCount = 30;
framesPerMove = 24;

outputFolder = FileNameJoin[{imageFolder, "rolling_animation_frames_" <> If[outer, "outer", "inner"]}];
If[!DirectoryQ[outputFolder], CreateDirectory[outputFolder]];

Needs["POVRayRender`"];
ConfigurePOVRayRender["POVRayPath" -> "C:\\Program Files\\POV-Ray\\v3.7\\bin\\pvengine64.exe"];

cubeVertices[dimension_] := 2 * Map[IntegerDigits[#, 2][[2;;dimension+1]]&, Table[k, {k, 2^dimension, 2 * 2^dimension - 1}]] - 1;
allPlusMinus[v_] := Union[Map[(v #)&, cubeVertices[Length[v]]]];
rotated[vertices_, center_, axis_, angle_] := Map[# + center &, Map[# - center &, vertices].RotationMatrix[angle, axis]];


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


drawImage[verticesIcosa_, edgeIndicesIcosa_, rotatedVerticesTetra_, edgeIndicesTetra_, serialNumber_] := Module[
    {thickness, range, lighting, graphElements, graph, outputFileName, cropCommand, },
    thickness = 0.07;
    range = 2.5;
    lighting = {{"Point", White, {10, -10, 10}}};
    graphElements = {};

    graphElements = Join[graphElements, Map[{FaceForm[Yellow], Sphere[#, 2 * thickness], thickness}&, verticesIcosa]];

    graphElements = Join[graphElements, Map[{FaceForm[Lighter[Red, 0.1]], Cylinder[verticesIcosa[[#]], thickness]}&, edgeIndicesIcosa]];

    graphElements = Join[graphElements, Map[{FaceForm[Yellow], Sphere[#, 2 * thickness], thickness}&, rotatedVerticesTetra]];

    graphElements = Join[graphElements, Map[{FaceForm[Magenta], Cylinder[rotatedVerticesTetra[[#]], thickness * 1.2]}&, edgeIndicesTetra]];


    graph = Graphics3D[
        graphElements,
        ViewPoint -> {3, -3, 0} * 10,
        ViewVertical -> {1, 0, phi},
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
    gifFileName = "random_rolling_" <> If[outer, "outer", "inner"];
    frameCount = stepCount * (1 + framesPerMove);
    gifFolder = imageFolder;
    gifCommand = "magick convert -quiet +repage -alpha remove -loop 0 -delay " <> IntegerString[Floor[100/framesPerMove]] <> " -dispose 2 " <> outputFolder <> "\\*.png " <> gifFolder <> "\\" <> gifFileName <> ".gif";

    Print[gifCommand];
    Run[gifCommand];
    Print["Saved the animation to " <> gifFolder <> "\\" <> gifFileName <> ".gif"];
];



(* initialization *)
phi = (Sqrt[5] + 1) / 2;
verticesSeedsIcosa = {
  {0, 1, phi},
  {1, phi, 0},
  {phi, 0, 1}
};

verticesIcosa = Union[Flatten[Map[allPlusMinus, verticesSeedsIcosa], 1]];
faceIndicesIcosa = getTriangularFaces[verticesIcosa];
edgeLength = Norm[verticesIcosa[[faceIndicesIcosa[[1]][[1]]]] - verticesIcosa[[faceIndicesIcosa[[1]][[2]]]]]//Simplify;
edgeIndicesIcosa = Union[Flatten[Map[Subsets[#, {2}]&, faceIndicesIcosa], 1]];

faceIndex = 1;
faceIndices1 = faceIndicesIcosa[[faceIndex]];
edgeIndices1 = faceIndices1[[{2, 3}]];
neighboringFaces = Select[faceIndicesIcosa, ContainsAll[#, edgeIndices1]&];

face1 = verticesIcosa[[neighboringFaces[[1]]]];
otherVertexIndex = Complement[neighboringFaces[[2]], edgeIndices1][[1]];
otherVertex = verticesIcosa[[otherVertexIndex]];
edgeCenter = Mean[verticesIcosa[[edgeIndices1]]];
edgeVector = verticesIcosa[[edgeIndices1]][[1]] - verticesIcosa[[edgeIndices1]][[2]];

face1Total = Total[face1];
kSolutions = Solve[Norm[k * face1Total - verticesIcosa[[faceIndicesIcosa[[1]][[1]]]]] == edgeLength, {k}]//FullSimplify;
ks = Sort[k/.kSolutions, #1<#2 &];
kFactor = ks[[ If[outer, 2, 1] ]];
apex1 = face1Total * kFactor//FullSimplify;

verticesTetra = Union[face1, {apex1}];
edgeIndicesTetra = Union[Flatten[Map[Subsets[#, {2}]&, {{1, 2, 3, 4}}], 1]];

v1 = apex1 - edgeCenter;
v2 = otherVertex - edgeCenter;
theta = ArcCos[(v1.v2)/Norm[v1]/Norm[v2]]//FullSimplify;

sign = Sign[Det[{edgeVector, edgeCenter, face1Total}]] * If[outer, 1, -1];
rotatedVerticesTetra = rotated[verticesTetra, edgeCenter, edgeVector, sign * theta/2];


(* random rolling *)
serialNumber = 1;
faceIndex = 1;
faceIndices1 = faceIndicesIcosa[[faceIndex]];
edgeIndices1 = faceIndices1[[{2, 3}]];

(* start loop *)
For[stepIndex = 1, stepIndex <= stepCount, stepIndex++,
    Print[{stepIndex, faceIndex, faceIndices1, edgeIndices1}];
    newFaceIndex = Select[Range[Length[faceIndicesIcosa]], ContainsAll[faceIndicesIcosa[[#]], edgeIndices1] && # != faceIndex &][[1]];

    edgeCenter = Mean[verticesIcosa[[edgeIndices1]]];
    edgeVector = verticesIcosa[[edgeIndices1]][[1]] - verticesIcosa[[edgeIndices1]][[2]];

    face1 = verticesIcosa[[faceIndices1]];
    face1Total = Total[face1];

    sign = Sign[Det[{edgeVector, edgeCenter, face1Total}]] * If[outer, 1, -1];

    For[frameIndex = 0, frameIndex < framesPerMove, frameIndex++,
        rotatedVerticesTetra = rotated[verticesTetra, edgeCenter, edgeVector, sign * theta / framesPerMove * frameIndex];
        drawImage[verticesIcosa, edgeIndicesIcosa, rotatedVerticesTetra, edgeIndicesTetra, serialNumber];
        serialNumber = serialNumber + 1;
    ];

    (* complete a rotation step *)
    verticesTetra = rotated[verticesTetra, edgeCenter, edgeVector, sign * theta]//FullSimplify;

    drawImage[verticesIcosa, edgeIndicesIcosa, verticesTetra, edgeIndicesTetra, serialNumber];
    serialNumber = serialNumber + 1;

    faceIndex = newFaceIndex;
    faceIndices1 = faceIndicesIcosa[[faceIndex]];
    allEdges = Subsets[faceIndices1, {2}];
    availableEdges = Select[allEdges, # != edgeIndices1 && # != edgeIndices1[[{2, 1}]] &];
    edgeIndices1 = availableEdges[[Random[Integer, {1, 2}]]];


];

exportAnimation[];

