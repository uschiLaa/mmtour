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


(* ::Input::Initialization:: *)
HiggsProjectionPlot[data1_,data2_:{{0,0}},xRange_:{0,0},yRange_:{0,0},cos_:{0.6,-0.05},tan_:{0.3,0.27},\[Lambda]1_:{0.0,0.6},\[Lambda]2_:{-0.3,0.3},\[Lambda]3_:{-0.6,0.05}, \[Lambda]4_:{-0.3,-0.3}, \[Lambda]5_:{0.,-0.6}]:=
DynamicModule[
{lst={cos,tan,\[Lambda]1,\[Lambda]2,\[Lambda]3,\[Lambda]4,\[Lambda]5}},
(*Grid formats the ouput of dynamic module*)
Grid[{{
LocatorPane[Dynamic[lst,({lst[[All, 1]], lst[[All,2]]}=Orthogonalize[{#[[All, 1]], #[[All, 2]]}])&],(*The line above preserves orthonormality*)
Graphics[
(*This part of the code creates the projected dimensions within a unit circle*)
{Circle[{0,0},1],
Dynamic[Arrow[{{0,0},lst[[1]]}]],Dynamic[ArrowText[lst[[1]], MathFont["cos(\[Beta]-\[Alpha])"]]],
Dynamic[Arrow[{{0,0},lst[[2]]}]],Dynamic[ArrowText[lst[[2]], MathFont["tan(\[Beta])"]]],
Dynamic[Arrow[{{0,0},lst[[3]]}]],Dynamic[ArrowText[lst[[3]], MathFont["\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\)"]]],
Dynamic[Arrow[{{0,0},lst[[4]]}]],Dynamic[ArrowText[lst[[4]], MathFont["\!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\)"]]],
Dynamic[Arrow[{{0,0},lst[[5]]}]],Dynamic[ArrowText[lst[[5]], MathFont["\!\(\*SubscriptBox[\(\[Lambda]\), \(3\)]\)"]]],
Dynamic[Arrow[{{0,0},lst[[6]]}]],Dynamic[ArrowText[lst[[6]], MathFont["\!\(\*SubscriptBox[\(\[Lambda]\), \(4\)]\)"]]],
Dynamic[Arrow[{{0,0},lst[[7]]}]],Dynamic[ArrowText[lst[[7]], MathFont["\!\(\*SubscriptBox[\(\[Lambda]\), \(5\)]\)"]]]
},Frame->True,ImageSize->165],Appearance->None],
(*This creates the scatter plot of the projected data*)
Dynamic[ListPlot[
(*The projection matrix is being applied to the data*)
{data1 . lst,If[data2=={{0,0}},Nothing,data2 . lst]},
PlotStyle->{Black, Red},
AxesOrigin->{0,0},
PlotRange->If[xRange=={0,0}\[And]yRange=={0,0},Automatic,{xRange, yRange}],
LabelStyle->FontFamily->"Times New Roman",
PerformanceGoal->"Speed",
AxesLabel->{"\!\(\*SubscriptBox[\(P\), \(1\)]\)", "\!\(\*SubscriptBox[\(P\), \(2\)]\)"},
ImageSize->675
]]},{"Proj. Matrix:" Dynamic[Text[ SetPrecision[lst//MatrixForm,3]]]}
},Background->Lighter[Gray,0.975],Frame->True]]


(* ::Input::Initialization:: *)
HiggsProjectionPlot::usage=
"An dynamic plot of the projected data onto a 2D plane. The projection matrix can be 
determined and applied to the data by moving the projected axes/dimensions within
the unit circle. Orthonormality of the projection matrix is preserved within this dynamic.
HiggsProjectionPlot[data1, data2(={{0,0}}), xRange, yRange, cos, tan, \[Lambda]1, \[Lambda]2, \[Lambda]3, \[Lambda]4, \[Lambda]5]
data1: A data matrix which consists of 7 dimensions. The dimensions need to be in the following order:
         (cos(\[Beta]-\[Alpha]), tan(\[Beta]), \[Lambda]1, \[Lambda]2, \[Lambda]3, \[Lambda]4, \[Lambda]5)
data2: Same as data1
xRange: A list which details the range of the horizontal axis.
yRange: A list which details the range of the vertical axis.
cos: A vector which represents the projected axis/dimension of this parameter.
tan: Same as cos.
\[Lambda]1:  Same as cos.
\[Lambda]2:  Same as cos.
\[Lambda]3:  Same as cos. 
\[Lambda]4:  Same as cos.
\[Lambda]5:  Same as cos.";


(* ::Input::Initialization:: *)
ProjectionPlot[data_,projmat_,xRange_:Automatic,yRange_:Automatic,colour_:Automatic]:=
DynamicModule[
{lst=If[projmat=="random",RandomMatrix[data[[1]]],projmat],arrowData,legendData},
arrowData=Reap[Sow[Circle[{0,0},1]];Do[With[{i=i},
Sow[Dynamic[Arrow[{{0,0},lst[[i]]}]]];
Sow[Dynamic[Text[StringForm["\!\(\*
StyleBox[\"x\",\nFontWeight->\"Plain\"]\)``",i], lst[[i]]+lst[[i]]/20]]];],{i,1,Length[data[[1, 1]]]}]][[2, 1]];
legendData=Reap[Do[Sow[Text[StringForm["data``",i]]],{i, 1, Length[data]}]][[2, 1]];
(*Grid formats the ouput of dynamic module*)
Grid[{{
(*The line above preserves orthonormality*)
LocatorPane[Dynamic[lst,({lst[[All, 1]], lst[[All,2]]}=Orthogonalize[{#[[All, 1]], #[[All, 2]]}])&],
(*This part of the code creates the projected dimensions within a unit circle*)
Graphics[arrowData,Frame->True,ImageSize->165],Appearance->None],
(*This creates the scatter plot of the projected data*)
Dynamic[ListPlot[
Map[Function[x, x . lst],data],
(*The projection matrix is being applied to the data*)
PlotStyle->colour,
AxesOrigin->{0,0},
PlotRange->{xRange,yRange},
LabelStyle->FontFamily->"Times New Roman",
PlotLegends->Placed[legendData,Above],
PerformanceGoal->"Speed",
AxesLabel->{"\!\(\*SubscriptBox[\(P\), \(1\)]\)", "\!\(\*SubscriptBox[\(P\), \(2\)]\)"},
ImageSize->675
]]},
{"Proj. Matrix:" Dynamic[Text[lst//MatrixForm]], SpanFromLeft}
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
Projected2DSliderPlot[data1_,data2_:{{0,0}},cos_:{0.6,-0.05},tan_:{0.3,0.27},\[Lambda]1_:{0.0,0.6},\[Lambda]2_:{-0.3,0.3},\[Lambda]3_:{-0.6,0.05}, \[Lambda]4_:{-0.3,-0.3}, \[Lambda]5_:{0.,-0.6}]:=
DynamicModule[
{lst={cos, tan, \[Lambda]1, \[Lambda]2, \[Lambda]3, \[Lambda]4, \[Lambda]5}},
Grid[{(*This portion creates the titles for the 2D sliders*)
{MathFont["cos(\[Beta]-\[Alpha])"],MathFont["tan(\[Beta])"],MathFont["\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\)"],
MathFont["\!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\)"],MathFont["\!\(\*SubscriptBox[\(\[Lambda]\), \(3\)]\)"],MathFont["\!\(\*SubscriptBox[\(\[Lambda]\), \(4\)]\)"],MathFont["\!\(\*SubscriptBox[\(\[Lambda]\), \(5\)]\)"]},
(*This portion creates the 2D sliders while ensuring the projection matrix is orthonormal*)
{LocatorPane[Dynamic[lst[[1]],({lst[[All, 1]], lst[[All, 2]]}=Orthogonalize[
{Join[{#[[1]]}, lst[[2;;7, 1]]], Join[{#[[2]]}, lst[[2;;7, 2]]]}])&],Graphics[{Circle[{0,0},1],Dynamic[Arrow[{{0,0},lst[[1]]}]]},ImageSize->120,Axes->True],Appearance->None],
LocatorPane[Dynamic[lst[[2]],({lst[[All, 1]], lst[[All, 2]]}=Orthogonalize[
{Join[{lst[[1, 1]]}, {#[[1]]},lst[[3;;7, 1]]], Join[{lst[[1, 2]]}, {#[[2]]},lst[[3;;7, 2]]]}])&],Graphics[{Circle[{0,0},1],Dynamic[Arrow[{{0,0},lst[[2]]}]]},ImageSize->120,Axes->True],Appearance->None],
LocatorPane[Dynamic[lst[[3]],({lst[[All, 1]], lst[[All, 2]]}=Orthogonalize[
{Join[lst[[1;;2, 1]], {#[[1]]},lst[[4;;7, 1]]], Join[lst[[1;;2, 2]], {#[[2]]},lst[[4;;7, 2]]]}])&],Graphics[{Circle[{0,0},1],Dynamic[Arrow[{{0,0},lst[[3]]}]]},ImageSize->120,Axes->True],Appearance->None],
LocatorPane[Dynamic[lst[[4]],({lst[[All, 1]], lst[[All, 2]]}=Orthogonalize[
{Join[lst[[1;;3, 1]], {#[[1]]},lst[[5;;7, 1]]], Join[lst[[1;;3, 2]], {#[[2]]},lst[[5;;7, 2]]]}])&],Graphics[{Circle[{0,0},1],Dynamic[Arrow[{{0,0},lst[[4]]}]]},ImageSize->120,Axes->True],Appearance->None],
LocatorPane[Dynamic[lst[[5]],({lst[[All, 1]], lst[[All, 2]]}=Orthogonalize[
{Join[lst[[1;;4, 1]], {#[[1]]},lst[[6;;7, 1]]], Join[lst[[1;;4, 2]], {#[[2]]},lst[[6;;7, 2]]]}])&],Graphics[{Circle[{0,0},1],Dynamic[Arrow[{{0,0},lst[[5]]}]]},ImageSize->120,Axes->True],Appearance->None],
LocatorPane[Dynamic[lst[[6]],({lst[[All, 1]], lst[[All, 2]]}=Orthogonalize[
{Join[lst[[1;;5, 1]], {#[[1]]},{lst[[7, 1]]}], Join[lst[[1;;5, 2]], {#[[2]]},{lst[[7, 2]]}]}])&],Graphics[{Circle[{0,0},1],Dynamic[Arrow[{{0,0},lst[[6]]}]]},ImageSize->120,Axes->True],Appearance->None],
LocatorPane[Dynamic[lst[[7]],({lst[[All, 1]], lst[[All, 2]]}=Orthogonalize[
{Join[lst[[1;;6, 1]], {#[[1]]}], Join[lst[[1;;6, 2]], {#[[2]]}]}])&],Graphics[{Circle[{0,0},1],Dynamic[Arrow[{{0,0},lst[[7]]}]]},ImageSize->120,Axes->True],Appearance->None]},
(*This portion shows the vector representation of the projected dimension*)
{MathFont[Dynamic[SetPrecision[lst[[1]], 3]]], MathFont[Dynamic[SetPrecision[lst[[2]], 3]]],
MathFont[Dynamic[SetPrecision[lst[[3]],3]]], MathFont[Dynamic[SetPrecision[lst[[4]], 3]]], MathFont[Dynamic[SetPrecision[lst[[5]], 3]]], MathFont[Dynamic[SetPrecision[lst[[6]], 3]]], MathFont[Dynamic[SetPrecision[lst[[7]],3]]]},
(*This portion creates the 2D scatter*)
{Dynamic[
ListPlot[{data1 . lst, If[data2=={{0,0}},Nothing, data2 . lst]},
(*The line above is where the projection matrix gets applied to the data*)
PlotStyle->{Black, Red},
AxesLabel->{"\!\(\*SubscriptBox[\(P\), \(1\)]\)", "\!\(\*SubscriptBox[\(P\), \(2\)]\)"},
LabelStyle->FontFamily->"Times New Roman",
ImageSize->680]], SpanFromLeft}},Background->Lighter[Gray,0.975],Frame->True]
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
RegFunction[axis1_,axis2_,label1_,label2_,range1_,range2_]:=
DynamicModule[{\[Alpha], \[Beta], \[Lambda]1, \[Lambda]2, \[Lambda]3, \[Lambda]4, \[Lambda]5,v=246, tbi, sabi, x1, x2},
Grid[{
(*There's a better way to go about this, I want to try and remove the invalid sliders via list methods or something similar*)
{\[Beta][tbi_]=ArcTan[tbi];
\[Alpha][sabi_,tbi_]=\[Beta][tbi]-ArcCos[sabi];},(*This is calculating cos and tan*)
If[TrueQ[axis1==tbi\[Or]axis2==tbi],Nothing,{Slider[Dynamic[tbi], {-3, 3}], Text[Style["Tan[\[Beta]] =", FontFamily->"Times New Roman"]],InputField[Dynamic[tbi],ImageSize->Small]}],
If[TrueQ[axis1==sabi\[Or]axis2==sabi],Nothing,{Slider[Dynamic[sabi], {-1, 0.999}], Text[Style["Cos[\[Beta]-\[Alpha]] =", FontFamily->"Times New Roman"]],InputField[Dynamic[sabi],ImageSize->Small]}],
If[TrueQ[axis1==\[Lambda]1\[Or]axis2==\[Lambda]1],Nothing,{Slider[Dynamic[\[Lambda]1], {-5, 5}],Text[Style["\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\) =", FontFamily->"Times New Roman"]],InputField[Dynamic[\[Lambda]1],ImageSize->Small]}], 
If[TrueQ[axis1==\[Lambda]2\[Or]axis2==\[Lambda]2],Nothing,{Slider[Dynamic[\[Lambda]2], {-5, 5}],Text[Style["\!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\) =", FontFamily->"Times New Roman"]],InputField[Dynamic[\[Lambda]2],ImageSize->Small] }], 
If[TrueQ[axis1==\[Lambda]3\[Or]axis2==\[Lambda]3],Nothing,{Slider[Dynamic[\[Lambda]3], {-5, 5}], Text[Style["\!\(\*SubscriptBox[\(\[Lambda]\), \(3\)]\) =", FontFamily->"Times New Roman"]],InputField[Dynamic[\[Lambda]3],ImageSize->Small]}],If[TrueQ[axis1==\[Lambda]4\[Or]axis2==\[Lambda]4],Nothing,{Slider[Dynamic[\[Lambda]4], {-5, 5}],Text[Style["\!\(\*SubscriptBox[\(\[Lambda]\), \(4\)]\) =", FontFamily->"Times New Roman"]],InputField[Dynamic[\[Lambda]4],ImageSize->Small]}],If[TrueQ[axis1==\[Lambda]5\[Or]axis2==\[Lambda]5],Nothing,{Slider[Dynamic[\[Lambda]5], {-5, 5}], Text[Style["\!\(\*SubscriptBox[\(\[Lambda]\), \(5\)]\) =", FontFamily->"Times New Roman"]],InputField[Dynamic[\[Lambda]5],ImageSize->Small]}],
{Checkbox[Dynamic[x1]],"Positivity Condition"},
{Checkbox[Dynamic[x2]], "Unitarity Condition"},
{Dynamic[
RegionPlot[
{PositiveMasses[v, \[Alpha][sabi, tbi],\[Beta][tbi], \[Lambda]1, \[Lambda]2, \[Lambda]3, \[Lambda]4, \[Lambda]5]\[And]If[x1, !(NegPositivityCondition[\[Lambda]1, \[Lambda]2, \[Lambda]3, \[Lambda]4, \[Lambda]5]), True]\[And]If[x2, !(NegUnitarityCond[\[Lambda]1, \[Lambda]2, \[Lambda]3, \[Lambda]4, \[Lambda]5]), True]},
{axis1,range1[[1]], range1[[2]]},{axis2, range2[[1]],range2[[2]]},
PlotRange->{range1,range2},
FrameLabel->{label1,label2},
ImageSize->Medium,
PerformanceGoal->"Speed"
]
]}
}]]


(* ::Input::Initialization:: *)
RegFunction::usage="A function which details the a region which satisifies certain inequality constraints. The dimensions of
the plot are chosen, and then the values of the other paramters can be chosen afterwards.
RegFunction[axis1, axis2, label1, label2, range1, range2]
axis1: The first dimension to plot against.
axis2: The second dimension to plot against.
label1: A string which provides a label for axis1.
label2: A string which provides a label for axis2.
range1: A list which specifies the range for axis1.
raneg2: A list which specifies the range for axis2.";


(* ::Input::Initialization:: *)
VisualiseSliceDynamic[data_,projmat_,centrePoint_,height_,heightRange_,ptSize1_:0.005,ptSize2_:0.004,minDist_:0]:=
DynamicModule[{lst=If[projmat=="random",RandomMatrix[data],projmat],centre =centrePoint ,h=height,arrowData},
arrowData=Reap[Sow[Circle[{0,0},1]];Do[With[{i=i},
Sow[Dynamic[Arrow[{{0,0},lst[[i]]}]]];
Sow[Dynamic[Text[StringForm["\!\(\*
StyleBox[\"x\",\nFontWeight->\"Plain\"]\)``",i], lst[[i]]+lst[[i]]/20]]];],{i,1,Length[data[[1]]]}]][[2, 1]];

Grid[{{"Centre Point"},
{InputField[Dynamic[centre],FieldSize->15]},(*Grid formats the ouput of dynamic module*)
{"Slice Height"},
{Slider[Dynamic[h],heightRange]},
{Dynamic[h]},(*The line above preserves orthonormality*)
{LocatorPane[Dynamic[lst,({lst[[All, 1]], lst[[All,2]]}=Orthogonalize[{#[[All, 1]], #[[All, 2]]}])&],
Graphics[arrowData,Frame->True,ImageSize->165],Appearance->None],
Dynamic[Module[{tempData=data, v1,v2},
v1=Reap[Do[If[minDist<genDist[xPrime[data[[i]], lst], cPrime[centrePoint, lst]]<h, 
Sow[data[[i]]];tempData[[i]]=Nothing],{i,1,Length[data]}]][[2, 1]];
v2=tempData;
ListPlot[{If[Length[v1]==0,{0,0},v1 . lst],If[Length[v2]==0,{0,0},v2 . lst]},
AspectRatio->1,
PlotStyle->{{Black,Opacity->1,PointSize[ptSize1]},{Lighter[Blue],Opacity->0.5,PointSize[ptSize2]}},
AxesLabel->{"\!\(\*
StyleBox[\"P1\",\nFontSlant->\"Italic\"]\)", "\!\(\*
StyleBox[\"P2\",\nFontSlant->\"Italic\"]\)"},
LabelStyle->FontFamily->"Times New Roman",
AspectRatio->Automatic,
ImageSize->500,
PlotLegends->Placed[{"In Slice","Not in Slice"},Above]]
]
],SpanFromAbove}},Frame->True]]


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


RandomMatrix[data_] :=
    Module[{len = Length[data[[1]]], mat = {}},
        Do[mat = AppendTo[mat, {RandomReal[{-1, 1}],RandomReal[{-1, 1}]}], len];
        mat = Transpose[Orthogonalize[Transpose[mat]]];
        Return[mat]
    ]
    (*Probably can compile this function*)


(* ::Input::Initialization:: *)
SliceDynamic[data_,projmat_,centrePoint_,height_,heightRange_,ptSize1_:0.005,ptSize2_:0.004]:=
DynamicModule[{lst=If[projmat=="random",RandomMatrix[data[[1;;1, 2;;-1]]],projmat],
centre =centrePoint ,h=height,dataSets,arrowData},
arrowData=Reap[Sow[Circle[{0,0},1]];Do[With[{i=i},
Sow[Dynamic[Arrow[{{0,0},lst[[i]]}]]];
Sow[Dynamic[Text[StringForm["\!\(\*
StyleBox[\"x\",\nFontWeight->\"Plain\"]\)``",i], lst[[i]]+lst[[i]]/20]]];],{i,1,Length[data[[1]]] - 1}]][[2, 1]];
dataSets=CreateDataSets[data];

Grid[{{"Centre Point"},
{InputField[Dynamic[centre],FieldSize->15]},(*Grid formats the ouput of dynamic module*)
{"Slice Height"},
{Slider[Dynamic[h],heightRange]},
{Dynamic[h]},(*The line above preserves orthonormality*)
{LocatorPane[Dynamic[lst,({lst[[All, 1]], lst[[All,2]]}=Orthogonalize[{#[[All, 1]], #[[All, 2]]}])&],
Graphics[arrowData,Frame->True,ImageSize->165],Appearance->None],
Dynamic[Module[{slice={}, colours={}, sliceSet={}},

Do[Do[If[genDist[xPrime[dataSets[[i,j, 1;;- 2]], lst], cPrime[centre, lst]]<h,
slice = Append[slice, dataSets[[i,j,1;;-2]]];
],{j, 1, Length[dataSets[[i]]]}];

(*I'm ensuring that I won't have a list of empty lists. Multiply a single empty list, {}, with projmat is fine.*)
If[Length[slice]>0, 
sliceSet = Append[sliceSet, slice];
colours = Append[colours, {ColorData[97, dataSets[[i, 1, -1]]]}];];(*Here you could develop some colour function if you'd like. I'm just using the default scheme (ColorData[97, n]).*)

slice ={},
{i, 1, Length[dataSets]}];

ListPlot[Map[Function[x, x . lst], sliceSet],
AxesLabel->{"\!\(\*
StyleBox[\"P1\",\nFontSlant->\"Italic\"]\)", "\!\(\*
StyleBox[\"P2\",\nFontSlant->\"Italic\"]\)"},
PlotStyle->colours,
LabelStyle->FontFamily->"Times New Roman",
 AspectRatio->1,PlotMarkers->{\[FilledSmallCircle]},PlotRange->{{Min[Transpose[Drop[data,{},{-1}] . lst][[1]]],Max[Transpose[Drop[data,{},{-1}] . lst][[1]]]},{Min[Transpose[Drop[data,{},{-1}] . lst][[2]]],Max[Transpose[Drop[data,{},{-1}] . lst][[2]]]}},
ImageSize->500
]
]
],SpanFromAbove}},Frame->True]]


(* ::Input:: *)
(*VisualiseSliceDynamic2[data_,projmat_,centrePoint_,height_,heightRange_,ptSize1_:0.005,ptSize2_:0.004,minDist_:0]:=*)
(*DynamicModule[{lst=projmat,centre =centrePoint ,h=height,arrowData,legendData},*)
(*arrowData=Reap[Sow[Circle[{0,0},1]];Do[With[{i=i},*)
(*Sow[Dynamic[Arrow[{{0,0},lst[[i]]}]]];*)
(*Sow[Dynamic[Text[StringForm["\!\(\*StyleBox[\"x\",FontWeight->\"Plain\"]\)``",i], lst[[i]]+lst[[i]]/20]]];],{i,1,Length[projmat]}]][[2, 1]];*)
(**)
(*Grid[{{"Centre Point"},*)
(*{InputField[Dynamic[centre],FieldSize->15]},(*Grid formats the ouput of dynamic module*)*)
(*{"Slice Height"},*)
(*{Slider[Dynamic[h],heightRange]},*)
(*{Dynamic[h]},(*The line above preserves orthonormality*)*)
(*{LocatorPane[Dynamic[lst,({lst[[All, 1]], lst[[All,2]]}=Orthogonalize[{#[[All, 1]], #[[All, 2]]}])&],*)
(*Graphics[arrowData,Frame->True,ImageSize->165],Appearance->None],*)
(*Dynamic[Module[{tempData=data, v1,v2},*)
(*v1=Reap[Do[If[minDist<genDist[xPrime[data[[i]], lst], cPrime[centrePoint, lst]]<h, *)
(*Sow[data[[i]]];tempData[[i]]=Nothing],{i,1,Length[data]}]][[2]];*)
(*v2=tempData;*)
(*ListPlot[{If[Length[v1]==0,{0,0},v1[[1]] . lst],If[Length[v2]==0,{0,0},v2 . lst]},*)
(*AspectRatio->1,*)
(*PlotStyle->{{Black,Opacity->1,PointSize[ptSize1]},{Lighter[Blue],Opacity->0.05,PointSize[ptSize2]}},*)
(*AxesLabel->{"\!\(\*StyleBox[\"P1\",FontSlant->\"Italic\"]\)", "\!\(\*StyleBox[\"P2\",FontSlant->\"Italic\"]\)"},*)
(*LabelStyle->FontFamily->"Times New Roman",*)
(*AspectRatio->Automatic,*)
(*ImageSize->500,*)
(*PlotLegends->Placed[{"In Slice","Not in Slice"},Above]]*)
(*]*)
(*],SpanFromAbove}},Frame->True]]*)


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


(* ::Input::Initialization:: *)
VisualiseSlice[data_,projMat_,centerPt_,dist_,ptSize1_:0.005,ptSize2_:0.004,minDist_:0]:=
Module[{tempData=data,lst=If[projMat=="random",RandomMatrix[data],projmat] ,v1={ConstantArray[0., Length[data[[1]]]]},v2={ConstantArray[0.,Length[ data[[1]]]]},arrowData},
v1=Reap[Do[If[minDist<genDist[xPrime[data[[i]], lst], cPrime[centerPt,lst]]<dist, 
Sow[data[[i]]];tempData[[i]]=Nothing],{i,1,Length[data]}]][[2]];
v2=tempData;
arrowData=Reap[Sow[Circle[{0,0},1]];
Do[Sow[ Arrow[{{0,0},lst[[i]]}]];Sow[Style[Text[StringForm["\!\(\*
StyleBox[\"x\",\nFontWeight->\"Plain\"]\)``",i], lst[[i]]+Normalize[lst[[i]]]/20],{FontFamily->"Times New Roman",Black}]],{i,1,Length[lst]}]][[2,1]];
Grid[{{
ListPlot[{If[Length[v1]==0,{0,0},v1[[1]] . lst],If[Length[v2]==0,{0,0}, v2 . lst]},
AspectRatio->1,
PlotStyle->{{Black,Opacity->1,PointSize[ptSize1]},{Lighter[Blue],Opacity->0.5,PointSize[ptSize2]}},
AxesLabel->{"\!\(\*
StyleBox[\"P1\",\nFontSlant->\"Italic\"]\)", "\!\(\*
StyleBox[\"P2\",\nFontSlant->\"Italic\"]\)"},
LabelStyle->FontFamily->"Times New Roman",
AspectRatio->Automatic,
ImageSize->Large,
PlotLegends->Placed[{"In Slice","Not in Slice"},Above]],Graphics[arrowData,Frame->True,PlotLabel->"Direction of Projection"]
}}]]


(* ::Input::Initialization:: *)
VisualiseSlice::usage=
"A function which indicates which points exists within a certain slice and which don't.
VisualiseSlice[data, projMat, centerPt, dist, ptSize1(=0.005), ptSize2(=0.004), minDist(=0)]
data: A data matrix.
projMat: A projection matrix which details the projection plane/slice.
centrePt: A point which indicates the position of the slice.
ptSize1: A number indicating the point size of the points within the slice.
ptSize2: A number indicating the point size of the point which exist outside of the slice";


(* ::Input::Initialization:: *)
SlicePlot[data_, projMat_,centerPt_,dist_,colour_:Automatic,minDist_:0]:=
Module[{dataSlice={}, lst=If[projMat=="random",RandomMatrix[data[[1]]],projmat],arrowData,legendData},
Do[dataSlice=Append[dataSlice,Reap[Do[If[minDist<genDist[xPrime[data[[i, j]], lst], cPrime[centerPt, lst]]<dist, 
Sow[data[[i, j]]]],{j,1,Length[data[[i]]]}]][[2,1]] ],{i, 1, Length[data]}];

arrowData=Reap[Sow[Circle[{0,0},1]];
Do[Sow[ Arrow[{{0,0},lst[[i]]}]];Sow[Style[Text[StringForm["\!\(\*
StyleBox[\"x\",\nFontWeight->\"Plain\"]\)``",i], lst[[i]]+Normalize[lst[[i]]]/20],{FontFamily->"Times New Roman",Black}]],{i,1,Length[lst]}]][[2,1]];

legendData=Reap[Do[Sow[Style[Text[StringForm["data``",i]],{FontFamily->"Times New Roman"}]],
{i, 1, Length[data]}]][[2, 1]];

Grid[{{
ListPlot[MapThread[Dot, {dataSlice, ConstantArray[lst,Length[data]]}],
AspectRatio->1,
PlotStyle->colour,
AxesLabel->{"\!\(\*
StyleBox[\"P1\",\nFontSlant->\"Italic\"]\)", "\!\(\*
StyleBox[\"P2\",\nFontSlant->\"Italic\"]\)"},
LabelStyle->{FontFamily->"Times New Roman"},
ImageSize->Large,
PlotLegends->Placed[legendData,Above]],
Graphics[arrowData,Frame->True,PlotLabel->"Direction of Projection"]
}}]]


(* ::Input::Initialization:: *)
SlicePlot::usage=
"A function which gets a certain slice from all the data matrices and plots it on the same axes.
SlicePlot[data, projMat, centerPt, dist, colour(=Automatic), minDist(=0)]
data: A list of data matrices.
projMat: A projection matrix which describes the projection plane/slice.
centrePt: A point which details the position of the slice.
colour: A list where you can determine the colour of each data matrix.
";


EndPackage[];
