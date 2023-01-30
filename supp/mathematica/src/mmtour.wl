(* ::Package:: *)

BeginPackage["mmtour`"];


(* ::Section:: *)
(*Functions*)


(* ::Subsection:: *)
(*Dynamic Plots and Functions*)


(* ::Input::Initialization:: *)
MathFont[txt_]:=
Text[Style[txt, FontFamily->"Times New Roman"]]


(* ::Input::Initialization:: *)
MathFont::usage=
"A helper function which changes the font of the text to Times New Roman.
MathFont[txt]
txt: A string of text to change fonts.";


CreateDataSets[data_]:=
Module[{sortedData ={},lst={}, dataSets={}, val},
sortedData = Sort[data, #1[[-1]]<#2[[-1]]&];val=sortedData[[1,-1]];
val;
(*creating the data sets*)
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


CreateDataSets::usage=
"A helper function which sorts an input matrix into set of matrices based on the numeric entries
  of the last collumn.

\!\(\*
StyleBox[\"CreateDataSets\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"[\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"data\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"]\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"\\\\n\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"data\",\nFontSlant->\"Italic\"]\): A data matrix with numeric entries.";


DefineDataSets[data_, flagQ_] := If[TrueQ[flagQ == 1],
  If[StringQ[data[[1, 1]]],
    CreateDataSets[data[[2 ;; -1]]]
    ,
    CreateDataSets[data]
  ]
  ,
  If[StringQ[data[[1, 1]]],
    {data[[2 ;; -1]]}
    ,
    {data}
  ]
]

(*Should try and compile*)


DefineLegend[dataSets_, legendQ_, flagQ_] := Module[{legend = {}},
  If[TrueQ[legendQ == 1],
    Do[
      With[{i = i},
        AppendTo[legend, Style[dataSets[[i, 1, -(legendQ + flagQ)]], 
          FontFamily -> "Times New Roman"]]
      ]
      ,
      {i, 1, Length[dataSets]}
    ]
    ,
    Do[
      With[{i = i},
        AppendTo[legend, Style[StringForm["Index ``", i], FontFamily 
          -> "Times New Roman"]]
      ]
      ,
      {i, 1, Length[dataSets]}
    ]
  ];
  Return[legend]
]

(*Should try and compile*)


RandomMatrix[data_,col_] :=
(*This function creates a random matrix based on the dimension of data.
If the last column of data represents groups, then a 1 is used for col. Otherwise, 0 is used.*)
    Module[{len = Length[data[[1]]] - col, mat = {}},
        Do[AppendTo[mat, {RandomReal[{-1, 1}],RandomReal[{-1, 1}]}], len];
        Orthonormalise[mat]
    ]


RandomMatrix::usage=
"A helper functions which generates a random projection matrix for a data matrix.

\!\(\*
StyleBox[\"RandomMatrix\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"[\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"data\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"col\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"]\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"\\\\n\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"data\",\nFontSlant->\"Italic\"]\): A data matrix.
\!\(\*
StyleBox[\"col\",\nFontSlant->\"Italic\"]\): An integer indicating how many collumns should be disregarded when generating a data matrix.";


Orthonormalise=Compile[{{projMat,_Real, 2}},
  Return[Transpose[Orthogonalize[Transpose[projMat]]]],
  CompilationTarget->"C"];


Orthonormalise::usage =
" A simple helper function which orthonormalises a potential projection matrix with real entries.

\!\(\*
StyleBox[\"Orthonormalise\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"[\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"projMat\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"]\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"\\\\n\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"projMat\",\nFontSlant->\"Italic\"]\): A projection matrix with real entries.";


RangeHelper=Compile[{{data, _Real, 2}}, 
(*Finds appropriate range if there are no column labels*)
Max[Map[Norm, data[[All, 1;;-1]]]],
CompilationTarget->"C"];


ProjectionPlot[data_,projMat_,legendQ_:1, flagQ_:1,colourFunc_:ColorData[97]]:=
(* DynamicModule retains instances of the dynamic (local) variables, these variables declared
   within the first list of  DynamicModule. These dynamic variables then can be changed by dynamic expressions
   within the module and then all Dyanmic objects will be updated accordingly with the new instances of these 
   variables. For example, lst will be changed by user input, then it will be orthonormalised and 
   then the dynamic objects with an instance of lst will be updated. *)
DynamicModule[{
(*Below are the dyanmic (local) variables within the scope of DynamicModule.
-> tempData is data but has the column names removed if there are any.
-> lst is the projection matrix to be applied to tempdata (or dataSets).
-> arrowData stores the column names, the position of the column names, 
   the position of the heads of the arrows and the unit circle.
-> pntSize controls the point size of the projected data.
-> legend stores the names for the legends.
-> dataSets store a list of matrices that are sorted based on the colour index.
-> colours stores the colours that will be used for the plot based on the colour index.
-> range determines the "ideal" range for the plot. However, it is assumed the data is centred about the origin.*)
 tempData = If[StringQ[data[[1, 1]]], data[[2;;-1]], data],
 lst=If[TrueQ[projMat=="random"], RandomMatrix[data, legendQ + flagQ], projMat],
 range=If[StringQ[data[[1, 1]]],
 RangeHelper[data[[2;;-1,1;;-(1+ legendQ + flagQ)]]], 
 RangeHelper[data[[All, 1;;-(1+ legendQ + flagQ)]]]],
 arrowData={Circle[{0,0},1]}, legend = {}, colours = {}, dataSets,
 pntSize=0.01, showMat = False, zoom = 0
},

(*This creates the neccesary data for arrowData. The commands Arrow and Text are encased within the function
 Dynamic as these objects will need to be updated when there are any changes to the  projection matrix lst. 
 Also, there is a command "With" within Do. This creates a local variable, i (different from i) in this case,
 within the scope of "With" and allows us to use every row of lst rather than lst[[i]].*)
If[StringQ[data[[1, 1]]], 
Do[With[{i=i},
AppendTo[arrowData, Dynamic[Arrow[{{0,0}, lst[[i]]}]]];
AppendTo[arrowData, Dynamic[Text[Style[data[[1, i]], FontFamily->"Times New Roman"], lst[[i]]+lst[[i]]/20]]];
],
{i,1,Length[data[[1]]]-(legendQ + flagQ)}],
Do[With[{i=i},
AppendTo[arrowData, Dynamic[Arrow[{{0,0},lst[[i]]}]]];
AppendTo[arrowData, Dynamic[Text[Style[StringForm["\!\(\*StyleBox[\"x\",\nFontWeight->\"Plain\"]\)``",i],
FontFamily->"Times New Roman"],lst[[i]]+lst[[i]]/20]]];
],
{i,1,Length[data[[1]]]-(legendQ + flagQ)}]];

(* This creates a list of matrices that are sorted by their colour index (if there is one),
   or puts the original matrix within a list. *)
If[TrueQ[flagQ==1],
dataSets=CreateDataSets[tempData],
dataSets = {tempData}];

(*This creates names for the legends if neccesary*)
If[TrueQ[legendQ==1],
Do[With[{i=i},
AppendTo[legend,
Style[dataSets[[i, 1, -(legendQ + flagQ)]], FontFamily->"Times New Roman"]]],{i, 1, Length[dataSets]}],
Do[With[{i = i},
AppendTo[legend,
Style[StringForm["Index ``",i], FontFamily->"Times New Roman"]]],
{i, 1, Length[dataSets]}]];

(*Creates a matrix where each row is associated with a matrix in dataSets. 
  Each data set is assigned a unique colour and each data set is utilising the same pointsize*)
Do[With[{i=i},
 AppendTo[colours, 
{If[ListQ[colourFunc],
colourFunc[[If[TrueQ[flagQ==1],dataSets[[i, 1, -1]],1]]], 
colourFunc[If[TrueQ[flagQ==1],dataSets[[i, 1, -1]],1]]],
PointSize[Dynamic[pntSize]]}]],
{i, 1, Length[dataSets]}];

(*Grid formats the ouput of DynamicModule, this is what the user can interact with*)
Grid[{{
 Column[{
Style["Point Size:" Dynamic[50 pntSize], FontFamily->"Times New Roman"],
Slider[Dynamic[pntSize],{0,0.02}],
Style["Zoom Scale:"Dynamic[Round[(zoom + 0.2)/(0.999 + 0.2),0.001]], FontFamily->"Times New Roman"],
Slider[Dynamic[zoom],{-0.2,0.999}],
(*- The line below preserves orthonormality of the projection matrix by extracting the the current values of
    the columns that were just changed by the user, then applying the gram-schmidt algorithm on the columns and
    then updating the columns of the dynamic variable lst. The values of the recently orthonormalised matrix are then
    used for the projection (and displayed if the user wants to see the current projection matrix.
  - LocatorPane creates a set of interactive points based on lst where each row is an interactive point.
  - The function Dynamic allows for lst to be updated and for a rule/function to be applied to lst
    (the syntax basic being Dynamic[lst, rule]).*)
LocatorPane[Dynamic[lst,({lst[[All, 1]], lst[[All,2]]}=Orthogonalize[{#[[All, 1]], #[[All, 2]]}])&],
(*This part of the code creates the main control module of the dynamic. The command Graphics displays 
  graphics primitives that are stored within arrowData.*)
Graphics[arrowData,Frame->True,ImageSize->165, PlotRange->{{-1, 1},{-1, 1}}],
{{-1, -1},{1, 1},{0.001, 0.001}},Appearance->None],
Grid[{{}}, ItemSize->{15, 1}],
Style["Proj. Matrix:"Dynamic[showMat], FontFamily->"Times New Roman"],
Checkbox[Dynamic[showMat]],
Dynamic[If[showMat,Grid[{{InputField[Dynamic[lst], FieldSize->15]}},ItemSize->{15, 13.5},Alignment->Top],
Grid[{{}}, ItemSize->{15, 13.5}]]]}, Center, 0.85],

(*This creates the scatter plot of the projected data. Once again, listplot is within Dynamic as it needs
  to be updated if there is a change in the projection matrix "lst" or colours.*)
Dynamic[
ListPlot[
(*Map and Function allow for each data Set to be multiplied with the projection matrix*)
Map[Function[x,
(*The if statement gets rid of some bugs with the legen when plotting some very particular data*)
If[MissingQ[x],
{Missing[]}, 
(*The projection matrix is being applied to the data*)
x[[All, 1;;-(1+legendQ + flagQ)]]] . lst], 
dataSets],
(*Below are commands which change how the data is displayed/looks*)
PlotStyle->colours,
PlotRange->{{-range (1 - zoom),range (1 - zoom)},{-range (1 - zoom) ,range (1 - zoom)}},
AspectRatio->1,
PlotLegends->Placed[PointLegend[legend,LegendMarkerSize->15,LegendMarkers->{{"\[FilledCircle]",15}}],Above],
LabelStyle->FontFamily->"Times New Roman",
(*Ensures that performance is optmised when displaying the projection/slice.
  Graphical "glitches" do happen if you are working with continuous lines/surfaces*)
PerformanceGoal->"Speed",
AxesLabel->{"\!\(\*SubscriptBox[\(P\), \(1\)]\)", "\!\(\*SubscriptBox[\(P\), \(2\)]\)"},
ImageSize->650
]]}},Frame->True]]


(* ::Input::Initialization:: *)
ProjectionPlot::usage=
"A dynamic plot of the projected data onto a 2D plane. The projection matrix can be 
changed and applied to the data by moving the projected dimensions within the
unit circle in the interactive. Orthonormality of the projection matrix is preserved
within this dynamic.

ProjectionPlot[\!\(\*
StyleBox[\"data\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"projMat\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"legendQ\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"(\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"=\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"1\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\")\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"flagQ\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"(\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"=\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"1\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\")\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"colorFunc\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"(\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"=\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"ColorData\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"[\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"97\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"]\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\")\",\nFontSlant->\"Italic\"]\)]
\!\(\*
StyleBox[\"data\",\nFontSlant->\"Italic\"]\): A data matrix where the second last column can have legend names as strings and the 
last column can have flag indices.
\!\(\*
StyleBox[\"projMat\",\nFontSlant->\"Italic\"]\): A projection matrix which describes the projection plane, the key word 'random' can
be used to generate a random projection matrix.
\!\(\*
StyleBox[\"legendQ\",\nFontSlant->\"Italic\"]\): 1 indicates that the second last column contains legend names, 0 indicates that    
there are no legend names.
\!\(\*
StyleBox[\"flagQ\",\nFontSlant->\"Italic\"]\): 1 indicates that the last column contains the group indices, 0 indicates that  
there are no flag indices.
\!\(\*
StyleBox[\"colorFunc\",\nFontSlant->\"Italic\"]\): A list of colours or the inbuilt ColorData Function, ColorData[n], to be used on the data.
The first colour will correspond to the flag of the lowest index and so on.";


(* ::Input::Initialization:: *)
ProjectedLocatorPlot[data_,projMat_, legendQ_:1,flagQ_:1, colourFunc_:ColorData[97]]:=
(*This function is similar to ProjectionPlot except the main cotrol module is different;      
 Each row of the projection matrix has its own unit circle. This sort of implementation may be easier in another language*)
DynamicModule[{
tempData =If[StringQ[data[[1, 1]]], data[[2;;-1]], data],lst=If[TrueQ[projMat=="random"], RandomMatrix[data, legendQ + flagQ], projMat],
range=If[StringQ[data[[1, 1]]],
RangeHelper[data[[2;;-1,1;;-(1+ legendQ + flagQ)]]],
RangeHelper[data[[All, 1;;-(1+ legendQ + flagQ)]]]],
locatorLst={}, txtLst={},dataSets={},legend={}, colours={},
pntSize=0.005,zoom=0,showMat = False
},

If[StringQ[data[[1, 1]]],
Do[With[{i= i},AppendTo[txtLst,Style[data[[1, i]],FontFamily->"Times New Roman"]]],{i, 1, Length[data[[1]]]-(legendQ+flagQ)}],Do[With[{i = i}, AppendTo[txtLst,Style[ StringForm["x``", i], FontFamily->"Times New Roman"]]],{i, 1, Length[data[[1]]]-(legendQ+flagQ)}]];

(* This portion creates the locator panes for the Dyanmic. Each row of the projection matrix has
there own locator pane, and the projection matrix is being orthonormalised by the line Orthogonalize[{ReplacePart[lst, n->#][[All, 1]],ReplacePart[lst, n->#][[All, 2]]}]
This rule allows for row n to be replaced by the user input. Then the matrix is orthonormalised and then
lst is then updated.*)
Do[
With[{n = i},AppendTo[locatorLst,LocatorPane[Dynamic[lst[[n]],
({lst[[All, 1]], lst[[All, 2]]}=
Orthogonalize[{ReplacePart[lst, n->#][[All, 1]],ReplacePart[lst, n->#][[All, 2]] }])& ],
Graphics[{Circle[{0,0},1],Dynamic[Arrow[{{0,0},lst[[n]]}]]},ImageSize->120,Axes->True],
Appearance->None]]
],
{i, 1, Length[data[[1]]] - (legendQ+flagQ)}];

AppendTo[locatorLst,SpanFromAbove];

If[TrueQ[flagQ==1],
dataSets=CreateDataSets[tempData],
dataSets = {tempData}];

Do[With[{i=i},
 AppendTo[colours, 
{If[ListQ[colourFunc],
colourFunc[[If[TrueQ[flagQ==1],dataSets[[i, 1, -1]],1]]], 
colourFunc[If[TrueQ[flagQ==1],dataSets[[i, 1, -1]],1]]],
PointSize[Dynamic[pntSize]]}]],
{i, 1, Length[dataSets]}];

If[TrueQ[legendQ==1],
Do[With[{n=i},
AppendTo[legend,Style[dataSets[[i, 1, -(legendQ + flagQ)]], FontFamily->"Times New Roman"]]],
{i, 1, Length[dataSets]}],
Do[With[{n = i},
AppendTo[legend,Style[StringForm["Index ``",i], FontFamily->"Times New Roman"]]],
{i, 1, Length[dataSets]}]];

Grid[{
txtLst,
locatorLst,
{Style["Size of Points:" Dynamic[pntSize 50], FontFamily->"Times New Roman"],SpanFromLeft,Style["Zoom Scale:"Dynamic[Round[(zoom + 0.2)/(0.999 + 0.2),0.001]],FontFamily->"Times New Roman"],SpanFromLeft, Grid[{{Style["Show Proj. Matrix:" Dynamic[showMat], FontFamily->"Times New Roman"]}},ItemSize->{15, Automatic}]},
{Slider[Dynamic[pntSize], {0,0.02}], SpanFromLeft,Slider[Dynamic[zoom],{-0.2,0.999}], SpanFromLeft, Checkbox[Dynamic[showMat]]},
{Dynamic[ListPlot[Map[Function[x,
If[
MissingQ[x],
{Missing[]}, 
x[[All, 1;;-(1+legendQ + flagQ)]]] . lst], 
dataSets],
PlotStyle->colours,
PlotRange->{{-range(1-zoom), range(1-zoom)}, {-range(1-zoom),range(1-zoom)}},
AspectRatio->1,
PlotLegends->Placed[PointLegend[legend,LegendMarkerSize->15,LegendMarkers->{{"\[FilledCircle]",15}}],Above],
AxesLabel->{"\!\(\*SubscriptBox[\(P\), \(1\)]\)", "\!\(\*SubscriptBox[\(P\), \(2\)]\)"},
LabelStyle->FontFamily->"Times New Roman",
PerformanceGoal->"Speed",
ImageSize->680]], SpanFromLeft},
{Dynamic[If[showMat,Grid[{{InputField[Dynamic[lst], FieldSize->15]}},ItemSize->{15, 13.5},
Alignment->Top],
Grid[{{}}, ItemSize->{15, 13.5}]]], SpanFromLeft}},Frame->True]
]


(* ::Input::Initialization:: *)
ProjectedLocatorPlot::usage=
"A dynamic plot of the projected data onto a 2D plane. This function is similar to ProjectionPlot,
however the main control module is slightly different; Each projected dimension of the data is
within a seperate locator pane which the user can interact with. Orthonormality of the projection 
matrix is preserved within this dynamic.

\!\(\*
StyleBox[\"ProjectedLocatorPlot\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"[\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"data\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"projMat\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"legendQ\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"(\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"=\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"1\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\")\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"flagQ\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"(\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"=\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"1\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\")\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"colorFunc\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"(\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"=\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"ColorData\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"[\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"97\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"]\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\")\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"]\",\nFontSlant->\"Italic\"]\)
\!\(\*
StyleBox[\"data\",\nFontSlant->\"Italic\"]\): A data matrix where the second last column can have legend names as strings and the 
last column can have flag indices.
\!\(\*
StyleBox[\"projMat\",\nFontSlant->\"Italic\"]\): A projection matrix which describes the projection plane, the key word 'random' can
be used to generate a random projection matrix.
\!\(\*
StyleBox[\"legendQ\",\nFontSlant->\"Italic\"]\): 1 indicates that the second last column contains legend names, 0 indicates that    
there are no legend names.
\!\(\*
StyleBox[\"flagQ\",\nFontSlant->\"Italic\"]\): 1 indicates that the last column contains the group indices, 0 indicates that  
there are no flag indices.
\!\(\*
StyleBox[\"colorFunc\",\nFontSlant->\"Italic\"]\): A list of colours or the inbuilt ColorData Function, ColorData[n], to be used on the data.
The first colour will correspond to the flag of the lowest index and so on.";


(* ::Input::Initialization:: *)
VisualiseSliceDynamic[data_,projMat_,height_,heightRange_]:=
(*This function allows you to visualise the slice by displaying what points exist within the slice
with one colour, and what points exist outside the slice in another colour.*)
DynamicModule[{
temp =If[StringQ[data[[1, 1]]],data[[2;;-1]], data],
lst=If[TrueQ[projMat=="random"],RandomMatrix[data, 0],projMat],centre =ConstantArray[0, Length[data[[1]]]] ,
range=If[StringQ[data[[1, 1]]], 
RangeHelper[data[[2;;-1]]],
RangeHelper[data]],
arrowData={Circle[{0,0},1]},h=height,pntSize1=0.005,
 pntSize2=0.004,showMat=False, zoom = 0
},

If[StringQ[data[[1, 1]]], 
Do[With[{i=i},
AppendTo[arrowData,Dynamic[Arrow[{{0,0},lst[[i]]}]]];AppendTo[arrowData, Dynamic[Text[Style[data[[1, i]], FontFamily->"Times New Roman"], lst[[i]]+lst[[i]]/20]]];],
{i,1,Length[data[[1]]]}],
Do[With[{i=i},
AppendTo[arrowData,Dynamic[Arrow[{{0,0},lst[[i]]}]]];AppendTo[arrowData, Dynamic[Text[Style[StringForm["\!\(\*StyleBox[\"x\",\nFontWeight->\"Plain\"]\)``",i],FontFamily->"Times New Roman"], lst[[i]]+lst[[i]]/20]]];],
{i,1,Length[data[[1]]]}]];

(*Grid formats the ouput of dynamic module*)
Grid[{{Column[{Style["Centre Point", FontFamily->"Times New Roman"], 
InputField[Dynamic[centre],FieldSize->14.5],
Style["Slice Height:"Dynamic[h], FontFamily->"Times New Roman"],
Slider[Dynamic[h],heightRange],
Style["Zoom Scale:"Dynamic[Round[(zoom + 0.2)/(0.999 + 0.2),0.001]],FontFamily->"Times New Roman"] ,
Slider[Dynamic[zoom],{-0.2, 0.999}],
Style["Size of Points in Slice",FontFamily->"Times New Roman"],
Slider[Dynamic[pntSize1],{0, 0.02}],
Style["Size of Points Outside of Slice",FontFamily->"Times New Roman"],
Slider[Dynamic[pntSize2],{0, 0.02}],
LocatorPane[Dynamic[lst,({lst[[All, 1]], lst[[All,2]]}=Orthogonalize[{#[[All, 1]], #[[All, 2]]}])&],
Graphics[arrowData,Frame->True,ImageSize->165, PlotRange->{{-1, 1},{-1, 1}}],Appearance->None],
Grid[{{}},ItemSize->{15, 2}],
Style["Proj. Matrix:"Dynamic[showMat],FontFamily->"Times New Roman"],
Checkbox[Dynamic[showMat]],
Dynamic[If[showMat,Grid[{{InputField[Dynamic[lst], FieldSize->15]}},ItemSize->{15, 13.5},
Alignment->Top],
Grid[{{}}, ItemSize->{15, 13.5}]]]}, Center, 0.7],

(*This creates the slice to be plotted. Module creates local variables*)
Dynamic[Module[{tempData=temp, v1={},v2},

(*The line below determines which points exist within the slice and
appends them to a lst called v1. The slice is being determined by
formulae provided in "Hollowness in high dimensional data" paper*)
Do[If[genDist[xPrime[temp[[i]], lst], cPrime[centre, lst]]<h, 
AppendTo[v1, temp[[i]]];tempData[[i]]=Nothing],{i,1,Length[temp]}];
v2=tempData;

(*This creates the plot showing the slice. ListPlot is already encased in Dyanmic*)
ListPlot[{If[Length[v1]==0,{Missing[]},v1 . lst],If[Length[v2]==0,{Missing[]},v2 . lst]},
AspectRatio->1,
PlotStyle->{{Black,Opacity->1,PointSize[pntSize1]},{Lighter[Blue],Opacity->0.5,PointSize[pntSize2]}},
PlotRange->{{-range(1-zoom), range(1-zoom)},{-range(1-zoom),range(1-zoom)}},
AxesLabel->{"\!\(\*
StyleBox[\"P1\",\nFontSlant->\"Italic\"]\)", "\!\(\*
StyleBox[\"P2\",\nFontSlant->\"Italic\"]\)"},
LabelStyle->FontFamily->"Times New Roman",
AspectRatio->1,
ImageSize->500,
PerformanceGoal->"Speed",
PlotLegends->Placed[{"In Slice","Not in Slice"},Above]]
]
]}},Frame->True]]


VisualiseSliceDynamic::usage=
"A function where you can plot a slice and manually change the specifications of the slice. Additionally,
this function allows you to see which points  exist within the slice and which exist out of the slice.

\!\(\*
StyleBox[\"SliceDynamic\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"[\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"data\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"projmat\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"height\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"heightRange\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"]\",\nFontSlant->\"Italic\"]\)
\!\(\*
StyleBox[\"data\",\nFontSlant->\"Italic\"]\): A data matrix (no flag or legend names should be in this matrix).
\!\(\*
StyleBox[\"projMat\",\nFontSlant->\"Italic\"]\): A projection matrix which describes the projection plane, the key word 'random' can
be used to generate a random projection matrix.
\!\(\*
StyleBox[\"height\",\nFontSlant->\"Italic\"]\): The initial height (or thickness) of the slice.
\!\(\*
StyleBox[\"heightRange\",\nFontSlant->\"Italic\"]\): A list with 2 entries where the first entry indicates the minimum 
thickness and the second entry indicates the maximum thickness.";


(* ::Input::Initialization:: *)
SliceDynamic[data_,projMat_,height_,heightRange_, legendQ_:1,flagQ_:1, colorFunc_:ColorData[97]]:=
DynamicModule[{
tempData =If[StringQ[data[[1, 1]]],data[[2;;-1]],data],
lst=If[TrueQ[projMat=="random"],RandomMatrix[data, legendQ + flagQ],projMat],
centre =ConstantArray[0,Length[data[[1]]]-( legendQ + flagQ)] ,
range=If[StringQ[data[[1, 1]]],
 RangeHelper[data[[2;;-1,1;;-(1+ legendQ + flagQ)]]], 
 RangeHelper[data[[All, 1;;-(1+ legendQ + flagQ)]]]], 
arrowData={Circle[{0,0},1]},legend={}, colours={},
zoom=0,h=height,dataSets, pntSize=0.0115,
boolVal = False,showMat=False
},

If[StringQ[data[[1, 1]]], 
Do[With[{i=i},
AppendTo[arrowData,Dynamic[Arrow[{{0,0},lst[[i]]}]]];AppendTo[arrowData, Dynamic[Text[Style[data[[1, i]], FontFamily->"Times New Roman"], lst[[i]]+lst[[i]]/20]]];],
{i,1,Length[data[[1]]]-(legendQ + flagQ)}],
Do[With[{i=i},
AppendTo[arrowData,Dynamic[Arrow[{{0,0},lst[[i]]}]]];AppendTo[arrowData, Dynamic[Text[Style[StringForm["\!\(\*StyleBox[\"x\",\nFontWeight->\"Plain\"]\)``",i], FontFamily->"Times New Roman"], lst[[i]]+lst[[i]]/20]]];],
{i,1,Length[data[[1]]]-(legendQ + flagQ)}]];

If[TrueQ[flagQ==1],
dataSets=CreateDataSets[tempData],
dataSets = {tempData}
];

If[TrueQ[legendQ==1],
Do[With[{i=i},
AppendTo[legend,
Style[dataSets[[i, 1, -(legendQ + flagQ)]], FontFamily->"Times New Roman"]]],{i, 1, Length[dataSets]}],
Do[With[{i = i},
AppendTo[legend,
Style[StringForm["Index ``",i], FontFamily->"Times New Roman"]]],
{i, 1, Length[dataSets]}
]
];

Do[With[{n=i},
 AppendTo[colours, 
{If[ListQ[colorFunc],
colorFunc[[If[TrueQ[flagQ==1],dataSets[[i, 1, -1]],1]]], 
colorFunc[If[TrueQ[flagQ==1],dataSets[[i, 1, -1]],1]]],
PointSize[Dynamic[pntSize]]}]],
{i, 1, Length[dataSets]}];

Grid[{
{Column[{Style["Centre Point",FontFamily->"Times New Roman"],
InputField[Dynamic[centre],FieldSize->15],
Style["Slice Height:"Dynamic[h], FontFamily->"Times New Roman"], 
 Slider[Dynamic[h],heightRange],
Style["Zoom Scale:"Dynamic[Round[(zoom + 0.2)/(0.999 + 0.2),0.001]], FontFamily->"Times New Roman"],
Slider[Dynamic[zoom],{-0.2,0.999}],
Style["Point Size:" Dynamic[50 pntSize], FontFamily->"Times New Roman"],
Slider[Dynamic[pntSize],{0,0.02}],
Style["Projection:" Dynamic[boolVal], FontFamily->"Times New Roman"],
Checkbox[Dynamic[boolVal]], LocatorPane[Dynamic[lst,({lst[[All, 1]], lst[[All,2]]}=Orthogonalize[{#[[All, 1]], #[[All, 2]]}])&],Graphics[arrowData,Frame->True,ImageSize->165, PlotRange->{{-1, 1},{-1, 1}}],
{{-1, -1},{1, 1},{0.001, 0.001}},Appearance->None],
Grid[{{}},ItemSize->{15, 1}],
Style["Proj. Matrix:"Dynamic[showMat], FontFamily->"Times New Roman"],
Checkbox[Dynamic[showMat]],
Dynamic[If[showMat,Grid[{{InputField[Dynamic[lst], FieldSize->15]}},ItemSize->{15, 13.5},Alignment->Top],
Grid[{{}}, ItemSize->{15, 13.5}]]]}, Center, 0.85],

Dynamic[Module[{sliceSet ={}, slice={}},
Do[Do[If[genDist[xPrime[dataSets[[i,j, 1;;- (1 +  legendQ + flagQ)]], lst], cPrime[centre, lst]]<h,
AppendTo[slice, dataSets[[i,j,1;;-(1 + legendQ + flagQ)]]];
],{j, 1, Length[dataSets[[i]]]}];

(*I'm ensuring that I won't have a list of empty lists by using Missing[]*)
If[Length[slice]>0, 
AppendTo[sliceSet, slice],
AppendTo[sliceSet, Missing[]]
];

slice ={},
{i, 1, Length[dataSets]}];

(*The commmand Missing[] allows for empty sets to be plotted but there is still a legend present*)
ListPlot[
(*The if statement which determines whether a slice or a projection will be displayed*)
If[boolVal,
Map[Function[x,
If[MissingQ[x],
{Missing[]}, 
x[[All, 1;;-(1 +  legendQ + flagQ)]]] . lst], 
dataSets],
Map[Function[x,
If[
MissingQ[x], 
{Missing[]}, 
x . lst]], 
sliceSet]
],
AxesLabel->{"\!\(\*
StyleBox[\"P1\",\nFontSlant->\"Italic\"]\)", "\!\(\*
StyleBox[\"P2\",\nFontSlant->\"Italic\"]\)"},
PlotStyle->colours,
LabelStyle->FontFamily->"Times New Roman",
PlotLegends->Placed[PointLegend[legend,LegendMarkerSize->15,LegendMarkers->{{"\[FilledCircle]",15}}],Above],AspectRatio->1,
PlotRange->{{-range(1-zoom), range(1-zoom)},{-range(1-zoom), range(1-zoom)}},
PerformanceGoal->"Speed",
ImageSize->610]]
]}},
Frame->True]
]


(* ::Input::Initialization:: *)
SliceDynamic::usage="A function where you can plot a slice or projection and manually change the specifications of
the slice, such as the projection matrix, slice height and the slice positions.

\!\(\*
StyleBox[\"SliceDynamic\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"[\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"data\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"projMat\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"height\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"heightRange\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"legendQ\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"(\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"=\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"1\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\")\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"flagQ\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"(\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"=\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"1\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\")\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"colourFunc\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"(\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"=\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"ColorData\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"[\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"97\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"]\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\")\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"]\",\nFontSlant->\"Italic\"]\)
\!\(\*
StyleBox[\"data\",\nFontSlant->\"Italic\"]\): A data matrix of the form where the second last column has legend names as strings and the
last column has the group index.
\!\(\*
StyleBox[\"projMat\",\nFontSlant->\"Italic\"]\): The inital projection matrix. The keyword 'random' can be used to generated a random matrix.
\!\(\*
StyleBox[\"height\",\nFontSlant->\"Italic\"]\): The initial height (or thickness) of the slice.
\!\(\*
StyleBox[\"heightRange\",\nFontSlant->\"Italic\"]\): A list with 2 entries where the first entry indicates the minimum 
thickness and the second entry indicates the maximum thickness.
\!\(\*
StyleBox[\"legendQ\",\nFontSlant->\"Italic\"]\): 1 indicates that the second last column contains legend names, 0 indicates that there
are no legend names.
\!\(\*
StyleBox[\"flagQ\",\nFontSlant->\"Italic\"]\): 1 indicates that the last column contains the group indices,
0 indicates that there are no group indices.
\!\(\*
StyleBox[\"colourFunc\",\nFontSlant->\"Italic\"]\):A list of colours or the inbuilt ColorData Function, ColorData[n], to be used on the data.
The first colour will correspond to the flag of the lowest index and so on.";


SliceDynamicCC[data_,projMat_,height_,heightRange_, legendQ_:1,flagQ_:1, colorFunc_:ColorData[97]]:=
DynamicModule[{
lst=If[TrueQ[projMat=="random"],RandomMatrix[data, legendQ + flagQ],projMat],
(*centre/outline variables are used for the centre guide*)
centreLst =Array[{-0.625Sin[(2 \[Pi](# - 1.))/(Length[data[[1]]]-(legendQ + flagQ))],
0.625Cos[(2\[Pi](#- 1.))/(Length[data[[1]]]-(legendQ + flagQ))]}&,
Length[data[[1]]]-(legendQ + flagQ)],
centreCopy =Array[{-Sin[(2 \[Pi](# - 1.))/(Length[data[[1]]]-(legendQ + flagQ))],
Cos[(2\[Pi](#- 1.))/(Length[data[[1]]]-(legendQ + flagQ))]}&,
Length[data[[1]]]-(legendQ + flagQ)],
centreLines=Array[Line[{{0.,0.},{-Sin[(2 \[Pi](# - 1.))/(Length[data[[1]]]-(legendQ + flagQ))],
Cos[(2\[Pi](#- 1.))/(Length[data[[1]]]-(legendQ + flagQ))]}}]&,
Length[data[[1]]]-(legendQ + flagQ)],
outline = {},
centreText  ={},
range=If[StringQ[data[[1, 1]]],
RangeHelper[data[[2;;-1,1;;-(1+ legendQ + flagQ)]]], 
RangeHelper[data[[All, 1;;-(1+ legendQ + flagQ)]]]], 
arrowData={Circle[{0.,0.},1.]},legend={}, colours={},
zoom=0.,h=height//N,dataSets, pntSize=0.0115,
boolVal = False,showMat=False, displaycentre = {}},

(*Visuals for projection control*)
If[StringQ[data[[1, 1]]], 
Do[With[{i=i},
AppendTo[arrowData,Dynamic[Arrow[{{0.,0.},lst[[i]]}]]];AppendTo[arrowData, Dynamic[Text[Style[data[[1, i]], FontFamily->"Times New Roman"], lst[[i]]+lst[[i]]/20]]];AppendTo[centreText,Text[data[[1, i]], 1.2centreCopy[[i]]]]],
{i,1,Length[data[[1]]]-(legendQ + flagQ)}],
Do[With[{i=i},
AppendTo[arrowData,Dynamic[Arrow[{{0,0},lst[[i]]}]]];AppendTo[arrowData, Dynamic[Text[Style[StringForm["\!\(\*StyleBox[\"x\",\nFontWeight->\"Plain\"]\)``",i], FontFamily->"Times New Roman"],
lst[[i]]+lst[[i]]/20]]];AppendTo[centreText, Text[Style[StringForm["\!\(\*StyleBox[\"x\",\nFontWeight->\"Plain\"]\)``",i], FontFamily->"Times New Roman"],  1.2centreCopy[[i]]]]],
	{i,1,Length[data[[1]]]-(legendQ + flagQ)}]];

Do[With[{i=i},
AppendTo[outline,Dynamic[Line[{centreLst[[i]], centreLst[[If[i+1>Length[data[[1]]]-(legendQ + flagQ), 1, i+1]]]}]]]],{i,1,Length[data[[1]]]-(legendQ + flagQ)}];

dataSets = DefineDataSets[data, flagQ];

legend = DefineLegend[ dataSets, legendQ, flagQ];

Do[With[{n=i},
AppendTo[colours, 
{If[ListQ[colorFunc],
colorFunc[[If[TrueQ[flagQ==1],dataSets[[i, 1, -1]],1]]], 
colorFunc[If[TrueQ[flagQ==1],dataSets[[i, 1, -1]],1]]],
PointSize[Dynamic[pntSize]]}]],
{i, 1, Length[dataSets]}];

Grid[{{Column[{
LocatorPane[Dynamic[centreLst, 
(For[i = 1,i<=Length[centreCopy],i++,
centreLst[[i]]=#[[i]] . centreCopy[[i]] centreCopy[[i]];
If[#[[i]] . centreCopy[[i]]>1., centreLst[[i]] = centreCopy[[i]]];If[#[[i]] . centreCopy[[i]]<0.25,centreLst[[i]]=0.25{-Sin[(2 \[Pi](i - 1.))/(Length[data[[1]]]-(legendQ + flagQ))],Cos[(2\[Pi](i- 1.))/(Length[data[[1]]]-(legendQ + flagQ))]}]
])&],
Graphics[{EdgeForm[{Gray, Dashed}],White,Polygon[centreCopy], Polygon[3/4 centreCopy], Polygon[1/2 centreCopy],Polygon[1/4 centreCopy], Black,centreLines,centreText, PointSize[0.0325],Point[Dynamic[centreLst]],outline }], 
Appearance->None],

Style["Centre Point:", FontFamily->"Times New Roman"],

Grid[{{Style[Dynamic[displaycentre], FontFamily->"Times New Roman"]}},ItemSize->{15, 4},Alignment->Top],
Style["Slice Height:"Dynamic[h], FontFamily->"Times New Roman"], 
Slider[Dynamic[h],heightRange],
Style["Zoom Scale:"Dynamic[Round[(zoom + 0.2)/(0.999 + 0.2),0.001]], FontFamily->"Times New Roman"],
Slider[Dynamic[zoom],{-0.2,0.999}],
Style["Point Size:" Dynamic[50 pntSize], FontFamily->"Times New Roman"],
Slider[Dynamic[pntSize],{0,0.02}],
Style["Projection:" Dynamic[boolVal], FontFamily->"Times New Roman"],
Checkbox[Dynamic[boolVal]],
LocatorPane[Dynamic[lst,({lst[[All, 1]], lst[[All,2]]}=Orthogonalize[{#[[All, 1]], #[[All, 2]]}])&],Graphics[arrowData,Frame->True,ImageSize->165, PlotRange->{{-1, 1},{-1, 1}}],
{{-1, -1},{1, 1},{0.001, 0.001}},Appearance->None],

Grid[{{}},ItemSize->{15, 1}],
Style["Proj. Matrix:"Dynamic[showMat], FontFamily->"Times New Roman"],
Checkbox[Dynamic[showMat]],
Dynamic[If[showMat,Grid[{{InputField[Dynamic[lst], FieldSize->15]}},ItemSize->{15, 13.5},Alignment->Top],
Grid[{{}}, ItemSize->{15, 13.5}]]]}, Center, 0.85],



Dynamic[Module[{sliceSet ={}, slice={}, centre={}},

If[StringQ[data[[1, 1]]],
Do[With[{i=i},AppendTo[centre,
(0.75-(Norm[centreLst[[i]]]-0.25))/0.75 Min[data[[2;;-1, i]]]+ ((Norm[centreLst[[i]]]-0.25))/0.75 Max[data[[2;;-1, i]]]]] ,{i,1,Length[centreLst]}],
Do[With[{i=i},AppendTo[centre,
(0.75-(Norm[centreLst[[i]]]-0.25))/0.75 Min[data[[All, i]] ]+ ((Norm[centreLst[[i]]]-0.25))/0.75 Max[data[[All, i]]]]] ,
{i,1,Length[centreLst]}]
];

displaycentre = centre;

Do[Do[If[Re[genDist[xPrime[dataSets[[i,j, 1;;- (1 +  legendQ + flagQ)]], lst],cPrime[centre, lst]]]<h,
AppendTo[slice, dataSets[[i,j,1;;-(1 + legendQ + flagQ)]]];
],
{j, 1, Length[dataSets[[i]]]}];

(*I'm ensuring that I won't have a list of empty lists by using Missing[]*)
If[Length[slice]>0, 
AppendTo[sliceSet, slice],
AppendTo[sliceSet, Missing[]]
];

slice ={},
{i, 1, Length[dataSets]}];

(*The commmand Missing[] allows for empty sets to be plotted but there is still a legend present*)
ListPlot[
(*The if statement which determines whether a slice or a projection will be displayed*)
If[boolVal,Map[Function[x,If[MissingQ[x],{Missing[]}, x[[All, 1;;-(1 +  legendQ + flagQ)]]] . lst], dataSets],
Map[Function[x,If[MissingQ[x], {Missing[]}, x . lst]], sliceSet]],
AxesLabel->{"\!\(\*StyleBox[\"P1\",\nFontSlant->\"Italic\"]\)",
 "\!\(\*StyleBox[\"P2\",\nFontSlant->\"Italic\"]\)"},
PlotStyle->colours,
LabelStyle->FontFamily->"Times New Roman",
PlotLegends->Placed[PointLegend[legend,LegendMarkerSize->15,LegendMarkers->{{"\[FilledCircle]",15}}],Above],AspectRatio->1,
PlotRange->{{-range(1-zoom), range(1-zoom)},{-range(1-zoom), range(1-zoom)}},
PerformanceGoal->"Speed",
ImageSize->610]]]}},
Frame->True]
]


(* ::Subsection:: *)
(*Slicing Functions*)


(* ::Input::Initialization:: *)
cPrime=Compile[{{c,_Real,1},{projMat,_Real,2}},
c-(c . projMat[[All, 1]])projMat[[All, 1]]-(c . projMat[[All, 2]])projMat[[All, 2]], CompilationTarget->"C"];


(* ::Input::Initialization:: *)
cPrime::usage=
"A hepler function which determines the orthogonal component of the vector v with respect to the projection
plane described by projMat. v also dictates where the projection plane will be positioned.

\!\(\*
StyleBox[\"cPrime\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"[\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"c\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"projMat\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"]\",\nFontSlant->\"Italic\"]\)
\!\(\*
StyleBox[\"c\",\nFontSlant->\"Italic\"]\): A vector which describes the position of the projection plane (i.e. where you want to slice the data).
\!\(\*
StyleBox[\"projMat\",\nFontSlant->\"Italic\"]\): A projection matrix which describes the projection plane/slice.";


(* ::Input::Initialization:: *)
xPrime=Compile[{{xRow,_Real, 1},{projMat,_Real, 2}},
xRow-(xRow . projMat[[All, 1]])projMat[[All, 1]]-(xRow . projMat[[All, 2]])projMat[[All, 2]], CompilationTarget->"C"];


(* ::Input::Initialization:: *)
xPrime::usage=
"A helper function which determines the normal from the projection plane to the point xRow.

\!\(\*
StyleBox[\"xPrime\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"[\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"xRow\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"projMat\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"]\",\nFontSlant->\"Italic\"]\)
\!\(\*
StyleBox[\"xRow\",\nFontSlant->\"Italic\"]\): A point/an observation from the data matrix.
\!\(\*
StyleBox[\"projMat\",\nFontSlant->\"Italic\"]\): A projection matrix which describes the projection plane.";


(* ::Input::Initialization:: *)
genDist=Compile[{{xP,_Real,1},{cP,_Real,1}},
Sqrt[xP . xP+cP . cP-2xP . cP], CompilationTarget->"C"];


(* ::Input::Initialization:: *)
genDist::usage=
"A function which determines the generalised orthogonal distance between a point in space and
the projection plane. 

\!\(\*
StyleBox[\"genDist\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"[\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"xP\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"cP\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"]\",\nFontSlant->\"Italic\"]\)
\!\(\*
StyleBox[\"xP\",\nFontSlant->\"Italic\"]\): The xPrime function.
\!\(\*
StyleBox[\"cP\",\nFontSlant->\"Italic\"]\): The cPrime function.";


EndPackage[];
