#!/usr/bin/env wolframscript

faceIndicesIcosa = {{1, 3, 7}, {1, 3, 9}, {1, 4, 7}, {1, 4, 10}, {1, 9, 10}, {2, 5, 8}, {2, 5, 9}, {2, 6, 8}, {2, 6, 10}, {2, 9, 10}, {3, 5, 9}, {3, 5, 11}, {3, 7, 11}, {4, 6, 10}, {4, 6, 12}, {4, 7, 12}, {5, 8, 11}, {6, 8, 12}, {7, 11, 12}, {8, 11, 12}};

modifiedDodecahedralGraph = Graph[
    {
        {1, 3, 7} \[UndirectedEdge] {1, 4, 7},
        {1, 4, 7} \[UndirectedEdge] {1, 4, 10},
        {1, 4, 10} \[UndirectedEdge] {1, 9, 10},
        {1, 9, 10} \[UndirectedEdge] {1, 3, 9},
        {1, 3, 9} \[UndirectedEdge] {1, 3, 7},
        
        {1, 3, 7} \[UndirectedEdge] {3, 7, 11},
        {1, 4, 7} \[UndirectedEdge] {4, 7, 12},
        {1, 4, 10} \[UndirectedEdge] {4, 6, 10},
        {1, 9, 10} \[UndirectedEdge] {2, 9, 10},
        {1, 3, 9} \[UndirectedEdge] {3, 5, 9},

        {1, 3, 7} \[UndirectedEdge] {3, 7, 11},
        {1, 4, 7} \[UndirectedEdge] {4, 7, 12},
        {1, 4, 10} \[UndirectedEdge] {4, 6, 10},
        {1, 9, 10} \[UndirectedEdge] {2, 9, 10},
        {1, 3, 9} \[UndirectedEdge] {3, 5, 9},

        {3, 7, 11} \[UndirectedEdge] {7, 11, 12},
        {7, 11, 12} \[UndirectedEdge] {4, 7, 12},
        {4, 7, 12} \[UndirectedEdge] {4, 6, 12},
        {4, 6, 12} \[UndirectedEdge] {4, 6, 10},
        {4, 6, 10} \[UndirectedEdge] {2, 6, 10},
        {2, 6, 10} \[UndirectedEdge] {2, 9, 10},
        {2, 9, 10} \[UndirectedEdge] {2, 5, 9},
        {2, 5, 9} \[UndirectedEdge] {3, 5, 9},
        {3, 5, 9} \[UndirectedEdge] {3, 5, 11},
        {3, 5, 11} \[UndirectedEdge] {3, 7, 11},

        {7, 11, 12} \[UndirectedEdge] {8, 11, 12},
        {4, 6, 12} \[UndirectedEdge] {6, 8, 12},
        {2, 6, 10} \[UndirectedEdge] {2, 6, 8},
        {2, 5, 9} \[UndirectedEdge] {2, 5, 8},
        {3, 5, 11} \[UndirectedEdge] {5, 8, 11},

        {7, 11, 12} \[UndirectedEdge] {8, 11, 12},
        {4, 6, 12} \[UndirectedEdge] {6, 8, 12},
        {2, 6, 10} \[UndirectedEdge] {2, 6, 8},
        {2, 5, 9} \[UndirectedEdge] {2, 5, 8},
        {3, 5, 11} \[UndirectedEdge] {5, 8, 11},

        {8, 11, 12} \[UndirectedEdge] {6, 8, 12},
        {6, 8, 12} \[UndirectedEdge] {2, 6, 8},
        {2, 6, 8} \[UndirectedEdge] {2, 5, 8},
        {2, 5, 8} \[UndirectedEdge] {5, 8, 11},
        {5, 8, 11} \[UndirectedEdge] {8, 11, 12}
    }
];

euler = FindEulerianCycle[modifiedDodecahedralGraph];

For[i = 1, i <= Length[First[euler]], i++,
    highlight = HighlightGraph[modifiedDodecahedralGraph, Part[First[euler], 1 ;; i]];
    Export["output/euler_" <> IntegerString[i, 10, 3] <> ".png", highlight];
];

(* table = Table[HighlightGraph[modifiedDodecahedralGraph, Part[First[euler], 1 ;; i]], {i, Length[First[euler]]}];

Export["table.png", table];
 *)
graph = GraphPlot3D[
 modifiedDodecahedralGraph, 
 (* EdgeShapeFunction -> ({Cylinder[#1, 0.1]} &),  *)
 (* VertexShapeFunction -> ({Sphere[#, 0.2]} &), *)
 GraphLayout -> "SpringElectricalEmbedding"
 ];


(* graph = GraphPlot[
 modifiedDodecahedralGraph,
 GraphLayout -> "PlanarEmbedding"
 ];
 *)

Export["output/test_graph.png", graph];