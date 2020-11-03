#!/usr/bin/env wolframscript

imageFolder = "output";
imageSize = 150 * {4, 3};
exportToPov = False;
exportToPov = True;

doubleIndicesSet = {{1, 2}};
(* doubleIndicesSet = {{1, 1}, {2, 2}, {3, 3}, {1, 2}, {1, 3}, {2, 3}}; *)


Needs["POVRayRender`"];
ConfigurePOVRayRender["POVRayPath" -> "C:\\Program Files\\POV-Ray\\v3.7\\bin\\pvengine64.exe"];

flipTetra = Cycles[{{101, 102}, {103, 104}}];
rotateTetra = Cycles[{{102, 103, 104}}];

flipOcta = Cycles[{{201, 202}, {203, 206}, {205, 204}, {207, 208}}];
rotateOcta = Cycles[{{202, 205, 203}, {204, 206, 207}}];

flipIcosa = Cycles[{{319, 320}, {318, 313}, {311, 316}, {309, 314}, {310, 315}, {302, 306}, {308, 304}, {301, 305}, {317, 312}, {307, 303}}];
rotateIcosa = Cycles[{{319, 316, 313}, {318, 315, 312}, {311, 317, 314}, {309, 306, 303}, {310, 307, 304}, {308, 305, 302}}];

flips = {flipTetra, flipOcta, flipIcosa};
rotates = {rotateTetra, rotateOcta, rotateIcosa};

getRollingGroup[index1_, index2_] := Module[
    {flip1, flip2, rotate1, rotate2, coreFlip, setup, generators},
    flip1 = flips[[index1]];
    flip2 = Cycles[flips[[index2]][[1]] + 1000];
    rotate1 = rotates[[index1]];
    rotate2 = Cycles[rotates[[index2]][[1]] + 1000];
    coreFlip = PermutationProduct[flip1, flip2];
    setup = PermutationProduct[rotate1, rotate2];

    (* coreFlip = Apply[PermutationProduct, flips[[doubleIndices]]]; *)
    (* setup = Apply[PermutationProduct, rotates[[doubleIndices]]]; *)
    generators = {coreFlip, PermutationProduct[setup, coreFlip, setup, setup], PermutationProduct[setup, setup, coreFlip, setup]};
    PermutationGroup[generators]
];

For[i = 1, i <= Length[doubleIndicesSet], i++,
    doubleIndices = doubleIndicesSet[[i]];

    rollingGroup = Apply[getRollingGroup, doubleIndices];
    Print[GroupOrder[rollingGroup]];
    setup = Apply[PermutationProduct, rotates[[doubleIndices]]];

    Print[GroupElementQ[rollingGroup, setup]];

    cayley = CayleyGraph[rollingGroup, VertexSize -> 0.4];
    hamCycle = FindHamiltonianCycle[cayley];


    edges = EdgeList[cayley];
    visibleEdges = Select[edges, #[[1]] < #[[2]]&];

    thickness = 0.03;

    (* Continue[]; *)
    (* graph = Graph3D[cayley, VertexSize -> 0.4, VertexStyle -> Gray]; *)

    graph = GraphPlot3D[
     visibleEdges,
     EdgeShapeFunction -> ({Cylinder[#1, thickness]} &), 
     VertexShapeFunction -> ({Sphere[#, 2 * thickness]} &),
     GraphLayout -> "SpringElectricalEmbedding"
    ];



(*     edges = EdgeList[cayley] //

         Map[Style[#, Black, 
            Glow@First@PropertyValue[{cayley, #}, EdgeStyle]] &] //

        ReplaceAll[
         e : DirectedEdge[args___] /; EdgeQ[cayley, Reverse[e]] :> 
          Sort@UndirectedEdge[args]
         ] //
       DeleteDuplicatesBy[First];

    vertices = VertexList[cayley];

    g3d = Graph3D[vertices, edges,
      VertexStyle -> Directive[Black, Glow@GrayLevel[2/3]],
      VertexSize -> 1/2,
      EdgeShapeFunction -> (Tube[Line[#1], 0.05] &)
      ];

      graph = g3d;
 *)
    outputFileName = If[exportToPov, "pov", "native"] <> "_test_" <> IntegerString[doubleIndices[[1]]] <> "_" <> IntegerString[doubleIndices[[2]]] <> ".png";

    outputFileName = FileNameJoin[{imageFolder, outputFileName}];
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

    cayleyFileName = "cayley_" <> IntegerString[doubleIndices[[1]]] <> "_" <> IntegerString[doubleIndices[[2]]] <> ".png";

    cayleyFileName = FileNameJoin[{imageFolder, cayleyFileName}];

    Export[cayleyFileName, cayley];

];

