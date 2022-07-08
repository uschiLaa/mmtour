(* ::Package:: *)

BeginPackage["mmtour`"];


(* ::Section:: *)
(*Functions*)


(* ::Subsection:: *)
(*Dynamic Plots and Functions*)


(* ::Input::Initialization:: *)
ArrowText[p1_,txt_]:=
Text[txt, p1+{0.02,0.02}]


(* ::Input::Initialization:: *)
ArrowText::usage=
"A helper function which places text near the coordinate p1.
ArrowText[p1, txt]
p1: A coordinate which determines the position of the text.
txt: A string of text to be written.
";


(* ::Input::Initialization:: *)
MathFont[txt_]:=
Text[Style[txt, FontFamily->"Times New Roman"]]


(* ::Input::Initialization:: *)
MathFont::usage=
"A helper function which changes the font of the text to Times New Roman.
MathFont[txt]
txt: A string of text to change fonts.";


Clear[ProjectionPlot]
ProjectionPlot[data_,projmat_,colorFunc_:ColorData[97]]:=
DynamicModule[{tempData = If[StringQ[data[[1, 1]]], data[[2;;-1]], data],
 lst=If[TrueQ[projmat=="random"], RandomMatrix[If[StringQ[data[[1, 1]]], data[[2;;-1]], data], 1],projmat], arrowData, pntSize=0.01,legend={},
dataSets={}, colours={}, range=Max[Map[Norm, If[StringQ[data[[1, 1]]], data[[2;;-1, 1;;-2]], data[[All, 1;;-2]]]]], 
boolVal, checkbox={}},


If[StringQ[data[[1, 1]]], 
arrowData=Reap[Sow[Circle[{0,0},1]];
Do[With[{i=i},
Sow[Dynamic[Arrow[{{0,0},lst[[i]]}]]];
Sow[Dynamic[Text[StringForm[data[[1, i]]], lst[[i]]+lst[[i]]/20]]];],{i,1,Length[tempData[[1]]]-1}]][[2, 1]],

arrowData=Reap[Sow[Circle[{0,0},1]];
Do[With[{i=i},
Sow[Dynamic[Arrow[{{0,0},lst[[i]]}]]];
Sow[Dynamic[Text[StringForm["\!\(\*
StyleBox[\"x\",\nFontWeight->\"Plain\"]\)``",i], lst[[i]]+lst[[i]]/20]]];],{i,1,Length[tempData[[1]]]-1}]][[2, 1]]

];

dataSets=CreateDataSets[tempData];

boolVal=ConstantArray[True, Length[dataSets]];

Do[With[{n = i}, AppendTo[checkbox, Checkbox[Dynamic[boolVal[[n]]]]]],{i, 1, Length[dataSets]} ];

Do[With[{n=i}, AppendTo[legend,Style[StringForm["Index ``",i],
FontFamily->"Times New Roman"]]],{i, 1, Length[dataSets]}];

Do[With[{n = i}, AppendTo[colours, {colorFunc[dataSets[[n, 1, -1]]], PointSize[Dynamic[pntSize]]}]];,
{i, 1, Length[dataSets]}];


(*Grid formats the ouput of dynamic module*)
Grid[{{
Slider[Dynamic[pntSize],{0,0.02}]},
{Text[PointSize: Dynamic[pntSize 50]]},
checkbox,
{(*The line below preserves orthonormality*)
LocatorPane[Dynamic[lst,({lst[[All, 1]], lst[[All,2]]}=Orthogonalize[{#[[All, 1]], #[[All, 2]]}])&],
(*This part of the code creates the projected dimensions within a unit circle*)
Graphics[arrowData,Frame->True,ImageSize->165],Appearance->None],
(*This creates the scatter plot of the projected data*)
Dynamic[
ListPlot[Map[Function[x, Style[x . lst]], dataSets[[All,All,1;;-2]]],
(*The projection matrix is being applied to the data*)
PlotStyle->colours,
AxesOrigin->{0,0},
PlotRange->{{-range,range},{-range,range}},
AspectRatio->1,
PlotLegends->Placed[PointLegend[legend,LegendMarkerSize->15,LegendMarkers->{{"\[FilledCircle]",15}}],Above],
LabelStyle->FontFamily->"Times New Roman",
PerformanceGoal->"Speed",
AxesLabel->{"\!\(\*SubscriptBox[\(P\), \(1\)]\)", "\!\(\*SubscriptBox[\(P\), \(2\)]\)"},
ImageSize->650
]]},
{"Proj. Matrix:" Dynamic[Text[lst//MatrixForm]]}
},Background->Lighter[Gray,0.975],Frame->True]]


(* ::Input::Initialization:: *)
ProjectionPlot::usage=
"An dynamic plot of the projected data onto a 2D plane. The projection matrix can be 
determined and applied to the data by moving the projected axes/dimensions within
the unit circle. Orthonormality of the projection matrix is preserved within this dynamic.
ProjectionPlot[data, projmat, xRange(=Automatic), yRange(=Automatic), colour(=Automatic)]
data: A list of data matrices.
projmat: A projection matrix which describes the projection plane.
xRange: A list which details the range of the horizontal axis.
yRange: A list which details the range of the vertical axis.
colour: A list of graphics directives to be applied to each data matrix.\[IndentingNewLine]";


(* ::Input::Initialization:: *)
Projected2DSliderPlot[data_,projmat_, colorFunc_:ColorData[97]]:=
DynamicModule[
{lst=If[TrueQ[projmat=="random"],RandomMatrix[data, 1],projmat], locatorLst={}, txtLst={},dataSets={}, colours={},pntSize=0.005,legend={},range=Max[Map[Norm, data[[All, 1;;-2]]]]},
(*This portion creates the titles for the 2D sliders*)Do[With[{n = i},locatorLst=AppendTo[locatorLst,LocatorPane[Dynamic[lst[[n]],({lst[[All, 1]], lst[[All, 2]]}
=Orthogonalize[{ReplacePart[lst, n->#][[All, 1]],ReplacePart[lst, n->#][[All, 2]] }])& ],Graphics[{Circle[{0,0},1],Dynamic[Arrow[{{0,0},lst[[n]]}]]},ImageSize->120,Axes->True],Appearance->None]]],{i, 1, Length[data[[1]]] - 1}];
locatorLst=AppendTo[locatorLst,SpanFromAbove];



dataSets=CreateDataSets[data];
Do[colours = Append[colours, {colorFunc[dataSets[[i, 1, -1]]], PointSize[Dynamic[pntSize]]}];,
{i, 1, Length[dataSets]}];

Do[With[{n=i},txtLst=AppendTo[txtLst,Style[StringForm["\!\(\*SubscriptBox[\(x\), \(``\)]\)",i],FontFamily->"Times New Roman"]]],{i, 1, Length[data[[1]]] - 1}];

Do[With[{n=i},legend=AppendTo[legend,Style[StringForm["Index ``",i],FontFamily->"Times New Roman"]]],{i, 1, Length[dataSets]}];

Grid[{
txtLst,
locatorLst,
{Text["Size of Points"],SpanFromLeft},
{Slider[Dynamic[pntSize], {0,0.01}], SpanFromLeft},
{Dynamic[ListPlot[Map[Function[x, x[[All, 1;;-2]] . lst], dataSets],
PlotStyle->colours,
PlotRange->{{-range, range}, {-range,range}},
AspectRatio->1,
PlotLegends->Placed[PointLegend[legend,LegendMarkerSize->15,LegendMarkers->{{"\[FilledCircle]",15}}],Left],
AxesLabel->{"\!\(\*SubscriptBox[\(P\), \(1\)]\)", "\!\(\*SubscriptBox[\(P\), \(2\)]\)"},
LabelStyle->FontFamily->"Times New Roman",
ImageSize->680]], SpanFromLeft},
{"Proj. Matrix:" Dynamic[Text[lst//MatrixForm]]}},Background->Lighter[Gray,0.975],Frame->True]
]


(* ::Input::Initialization:: *)
Projected2DSliderPlot::usage=
"An dynamic plot of the projected data onto a 2D plane. The projection matrix can be 
determined and applied to the data by moving 2D sliders. Each slide corresponds to a 
projected dimension. The centre of the sliders is (0, 0), orthonormality of the
projection matrix is preserved within this dynamic.
Projected2DSliderPlot[data1, data2(={{0,0}}), cos, tan, \[Lambda]1, \[Lambda]2, \[Lambda]3, \[Lambda]4, \[Lambda]5]
data1: A data matrix which consists of 7 dimensions. The dimensions need to be in the following order:
        (cos(\[Beta]-\[Alpha]), tan(\[Beta]), \[Lambda]1, \[Lambda]2, \[Lambda]3, \[Lambda]4, \[Lambda]5)
data2: Same as data1
cos: A vector which represents the projected axis/dimension of this parameter.
tan: Same as cos.
\[Lambda]1: Same as cos.
\[Lambda]2: Same as cos.
\[Lambda]3: Same as cos. 
\[Lambda]4: Same as cos.
\[Lambda]5: Same as cos.
";


(* ::Input::Initialization:: *)
VisualiseSliceDynamic[data_,projmat_,centrePoint_,height_,heightRange_,minDist_:0]:=
DynamicModule[{lst=If[TrueQ[projmat=="random"],RandomMatrix[data, 0],projmat],centre =centrePoint ,h=height,arrowData, pntSize1=0.005, pntSize2=0.004,range=Max[Map[Norm, data[[All, 1;;-2]]]]},
arrowData=Reap[Sow[Circle[{0,0},1]];Do[With[{i=i},
Sow[Dynamic[Arrow[{{0,0},lst[[i]]}]]];
Sow[Dynamic[Text[StringForm["\!\(\*
StyleBox[\"x\",\nFontWeight->\"Plain\"]\)``",i], lst[[i]]+lst[[i]]/20]]];],{i,1,Length[data[[1]]]}]][[2, 1]];

Grid[{{"Centre Point"},
{InputField[Dynamic[centre],FieldSize->15]},(*Grid formats the ouput of dynamic module*)
{"Slice Height"},
{Slider[Dynamic[h],heightRange]},
{Dynamic[h]},(*The line above preserves orthonormality*)
{"Size of Points in Slice"},
{Slider[Dynamic[pntSize1],{0, 0.01}]},
{"Size of Points Outside of Slice"},
{Slider[Dynamic[pntSize2],{0, 0.01}]},
{LocatorPane[Dynamic[lst,({lst[[All, 1]], lst[[All,2]]}=Orthogonalize[{#[[All, 1]], #[[All, 2]]}])&],
Graphics[arrowData,Frame->True,ImageSize->165],Appearance->None],
Dynamic[Module[{tempData=data, v1,v2},
v1=Reap[Do[If[minDist<genDist[xPrime[data[[i]], lst], cPrime[centrePoint, lst]]<h, 
Sow[data[[i]]];tempData[[i]]=Nothing],{i,1,Length[data]}]][[2, 1]];
v2=tempData;
ListPlot[{If[Length[v1]==0,{0,0},v1 . lst],If[Length[v2]==0,{0,0},v2 . lst]},
AspectRatio->1,
PlotStyle->{{Black,Opacity->1,PointSize[pntSize1]},{Lighter[Blue],Opacity->0.5,PointSize[pntSize2]}},
PlotRange->{{-range, range},{-range,range}},
AxesLabel->{"\!\(\*
StyleBox[\"P1\",\nFontSlant->\"Italic\"]\)", "\!\(\*
StyleBox[\"P2\",\nFontSlant->\"Italic\"]\)"},
LabelStyle->FontFamily->"Times New Roman",
AspectRatio->1,
ImageSize->500,
PlotLegends->Placed[{"In Slice","Not in Slice"},Above]]
]
],SpanFromAbove},
{"Proj. Matrix:" Dynamic[Text[lst//MatrixForm]]}},Frame->True]]


(* ::Input::Initialization:: *)
CreateDataSets[data_]:=
Module[{sortedData ={},lst={}, dataSets={}, val},
sortedData = Sort[data, #1[[-1]]<#2[[-1]]&];
val=sortedData[[1,-1]];
val;
For[i = 1,i<=Length[data],i++,
If[val ==sortedData[[i, -1]],
lst = Append[lst, sortedData[[i]]],
val=sortedData[[i, -1]];
dataSets=Append[dataSets, lst];
lst = {sortedData[[i]]};
]
];
dataSets=Append[dataSets, lst];
Return[dataSets]
]


RandomMatrix[data_,col_] :=
    Module[{len = Length[data[[1]]] - col, mat = {}},
        Do[mat = AppendTo[mat, {RandomReal[{-1, 1}],RandomReal[{-1, 1}]}], len];
        mat = Transpose[Orthogonalize[Transpose[mat]]];
        Return[mat]
    ]
    (*Probably can compile this function*)


(* ::Input::Initialization:: *)
Clear[SliceDynamic]
SliceDynamic[data_,projmat_,centrePoint_,height_,heightRange_]:=
DynamicModule[{lst=If[TrueQ[projmat=="random"],RandomMatrix[data, 1],projmat],
centre =centrePoint ,h=height,dataSets,arrowData,pntSize=0.006, range=Max[Map[Norm, data[[All, 1;;-2]]]], legend={}},
arrowData=Reap[Sow[Circle[{0,0},1]];Do[With[{i=i},
Sow[Dynamic[Arrow[{{0,0},lst[[i]]}]]];
Sow[Dynamic[Text[StringForm["\!\(\*
StyleBox[\"x\",\nFontWeight->\"Plain\"]\)``",i], lst[[i]]+lst[[i]]/20]]];],{i,1,Length[data[[1]]] - 1}]][[2, 1]];
dataSets=CreateDataSets[data];

Do[With[{n=i},legend=AppendTo[legend,Style[StringForm["Index ``",i],FontFamily->"Times New Roman"]]],{i, 1, Length[dataSets]}];

Grid[{{"Centre Point"},
{InputField[Dynamic[centre],FieldSize->15]},(*Grid formats the ouput of dynamic module*)
{"Slice Height:"Dynamic[h]},
{Slider[Dynamic[h],heightRange]},
{"Point Size:" Dynamic[100 pntSize]},
{Slider[Dynamic[pntSize],{0,0.02}]},
{LocatorPane[Dynamic[lst,({lst[[All, 1]], lst[[All,2]]}=Orthogonalize[{#[[All, 1]], #[[All, 2]]}])&],
Graphics[arrowData,Frame->True,ImageSize->165],Appearance->None],
Dynamic[Module[{slice={}, colours={},sliceSet={}},

Do[Do[If[genDist[xPrime[dataSets[[i,j, 1;;- 2]], lst], cPrime[centre, lst]]<h,
slice = Append[slice, dataSets[[i,j,1;;-2]]];
],{j, 1, Length[dataSets[[i]]]}];

(*I'm ensuring that I won't have a list of empty lists. Multiply a single empty list, {}, with projmat is fine.*)
If[Length[slice]>0, 
sliceSet = Append[sliceSet, slice];
colours = Append[colours, {ColorData[97, dataSets[[i, 1, -1]]], PointSize[pntSize]}];];(*Here you could develop some colour function if you'd like. I'm just using the default scheme (ColorData[97, n]).*)

slice ={},
{i, 1, Length[dataSets]}];

ListPlot[Map[Function[x, x . lst], sliceSet],
AxesLabel->{"\!\(\*
StyleBox[\"P1\",\nFontSlant->\"Italic\"]\)", "\!\(\*
StyleBox[\"P2\",\nFontSlant->\"Italic\"]\)"},
PlotStyle->colours,
LabelStyle->FontFamily->"Times New Roman",
PlotLegends->Placed[PointLegend[legend,LegendMarkerSize->15,LegendMarkers->{{"\[FilledCircle]",15}}],Above],
 AspectRatio->1,PlotRange->{{-range, range},{-range, range}},
ImageSize->500
]
]
],SpanFromAbove},
{"Proj. Matrix:" Dynamic[Text[lst//MatrixForm]]}},Frame->True]]


(* ::Input::Initialization:: *)
VisualiseSliceDynamic::usage=
"A function where you can plot a slice and manually change the specifications of the slice such as the projection matrix, slice height and the slice positions.
SliceDynamic[data, projmat, centrePoint, height, heightRange, ptSize1(=0.005), ptSize2(=0.004), minDist(=0)]
data: A data matrix
projmat: The inital projection matrix
centrepoint: The initial position of the slice
height: The initial height of the slice
heightRange: The range of heights
ptSize1: The size of the points that exist within the slice
ptSize2: The size of the points that exist outside of the slice
";


(* ::Subsection:: *)
(*Slicing Functions*)


(* ::Input::Initialization:: *)
cPrime=Compile[{{c,_Real,1},{projMat,_Real,2}},
c-(c . projMat[[All, 1]])projMat[[All, 1]]-(c . projMat[[All, 2]])projMat[[All, 2]], CompilationTarget->"C"];


(* ::Input::Initialization:: *)
cPrime::usage=
"A hepler function which determines the orthogonal component of the vector v with respect to the projection
plane described by projMat. v also dictates where the projection plane will be positioned.
cPrime[c, projMat]
c: A vector which describes the position of the projection plane (i.e. where you want to slice the data)
projMat: A projection matrix which describes the projection plane/slice
";


(* ::Input::Initialization:: *)
xPrime=Compile[{{xRow,_Real, 1},{projMat,_Real, 2}},
xRow-(xRow . projMat[[All, 1]])projMat[[All, 1]]-(xRow . projMat[[All, 2]])projMat[[All, 2]], CompilationTarget->"C"];


(* ::Input::Initialization:: *)
xPrime::usage=
"A helper function which determines the normal from the projection plane to the point xRow.
xPrime[xRow, projMat]
xRow: A point/an observation from the data matrix
projMat: A projection matrix which describes the projection plane	
";


(* ::Input::Initialization:: *)
genDist=Compile[{{xP,_Real,1},{cP,_Real,1}},
Sqrt[xP . xP+cP . cP-2xP . cP], CompilationTarget->"C"];


(* ::Input::Initialization:: *)
genDist::usage=
"A function which determines the generalised orthogonal distance between a point in space and
the projection plane. 
genDist[xP, cP]
xP: Ideally the xPrime function
cP: Ideally the cPrime function
";


EndPackage[];
