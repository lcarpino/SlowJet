(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: durham *)

(* :Author: *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

BeginPackage["SlowJet`epa`durham`"]

  y23DurhamTwoJetX::usage = "y23DurhamTwoJetX[ycut, x1, x2] returns 1 if the point (x1, x2) is inside the two-jet region and 0 if the point (x1, x2) is outside the two-jet region"
  y23DurhamThreeJetX::usage = "y23DurhamThreeJetX[ycut, x1, x2] returns 1 if the point (x1, x2) is inside the three-jet region and 0 if the point (x1, x2) is outside the three-jet region"

  y23DurhamTwoJetRegionX::usage = "y23DurhamTwoJetRegionX[ycut] returns an implicit region of the two-jet phase space in terms of x1 and x2"
  y23DurhamThreeJetRegionX::usage = "y23DurhamThreeJetRegionX[ycut] returns an implicit region of the three-jet phase space in terms of x1 and x2"


  y23DurhamTwoJetY::usage = "y23DurhamTwoJetY[ycut, y13, y23] returns 1 if the point (y13, y23) is inside the two-jet region and 0 if the point (y13, y23) is outside the two-jet region"
  y23DurhamThreeJetY::usage = "y23DurhamThreeJetY[ycut, y13, y23] returns 1 if the point (y13, y23) is inside the three-jet region and 0 if the point (y13, y23) is outside the three-jet region"

  y23DurhamTwoJetRegionY::usage = "y23DurhamTwoJetRegionY[ycut] returns an implicit region of the two-jet phase space in terms of y13 and y23"
  y23DurhamThreeJetRegionY::usage = "y23DurhamThreeJetRegionY[ycut] returns an implicit region of the three-jet phase space in terms of y13 and y23"


  Begin["`Private`"]

    (* Durham algorithm in terms of x_i variables *)

    y23DurhamTwoJetX[ycut_, x1_, x2_] := Module[
      {x3=2-x1-x2},
      If[ x1 == 1 || x2 == 1 || x3 == 1,
        1,
        Boole[
          (Min[x1/x2, x2/x1] (1 - x3) < ycut ||
           Min[x1/x3, x3/x1] (1 - x2) < ycut ||
           Min[x2/x3, x3/x2] (1 - x1) < ycut) &&
          x1 + x2 >= 1]
        ]]

    y23DurhamThreeJetX[ycut_, x1_, x2_] := Module[
      {x3=2-x1-x2},
      (Boole[Min[x1/x2, x2/x1] (1 - x3) > ycut]
       Boole[Min[x1/x3, x3/x1] (1 - x2) > ycut]
       Boole[Min[x2/x3, x3/x2] (1 - x1) > ycut]
       Boole[x1 + x2 >= 1])]

    y23DurhamTwoJetRegionX[ycut_] := Module[
      {x1, x2, x3},
      x3 = 2 - x1 - x2;
      ImplicitRegion[
        (Min[x1/x2, x2/x1] (1 - x3) < ycut ||
         Min[x1/x3, x3/x1] (1 - x2) < ycut ||
         Min[x2/x3, x3/x2] (1 - x1) < ycut) &&
        x1 + x2 >= 1,
        {{x1, 0, 1}, {x2, 0, 1}} ]]

    y23DurhamThreeJetRegionX[ycut_] := Module[
      {x1, x2, x3},
      x3 = 2 - x1 - x2;
      ImplicitRegion[
        (Min[x1/x2, x2/x1] (1 - x3) > ycut &&
         Min[x1/x3, x3/x1] (1 - x2) > ycut &&
         Min[x2/x3, x3/x2] (1 - x1) > ycut),
        {{x1, 0, 1}, {x2, 0, 1}} ]]


    (* Durham algorithm in terms of y_ij variables *)

    y23DurhamTwoJetY[ycut_, y13_, y23_] := Module[
      {y12=1-y13-y23},
      If[ y12 == 1 || y13 == 1 || y23 == 1,
        1,
        Boole[
          (Min[(y12+y13)/(y12+y23), (y12+y23)/(y12+y13)] y12 < ycut ||
           Min[(y12+y13)/(y13+y23), (y13+y23)/(y12+y13)] y13 < ycut ||
           Min[(y12+y23)/(y13+y23), (y13+y23)/(y12+y23)] y23 < ycut) &&
          y13 + y23 <= 1]
        ]]

    y23DurhamThreeJetY[ycut_, y13_, y23_] := Module[
      {y12=1-y13-y23},
      (Boole[Min[(y12+y13)/(y12+y23), (y12+y23)/(y12+y13)] y12 > ycut]
       Boole[Min[(y12+y13)/(y13+y23), (y13+y23)/(y12+y13)] y13 > ycut]
       Boole[Min[(y12+y23)/(y13+y23), (y13+y23)/(y12+y23)] y23 > ycut]
       Boole[y13 + y23 <= 1])]

    y23DurhamTwoJetRegionY[ycut_] := Module[
      {y12, y13, y23},
      y12 = 1 - y13 - y23;
      ImplicitRegion[
        (Min[(y12+y13)/(y12+y23), (y12+y23)/(y12+y13)] y12 < ycut ||
         Min[(y12+y13)/(y13+y23), (y13+y23)/(y12+y13)] y13 < ycut ||
         Min[(y12+y23)/(y13+y23), (y13+y23)/(y12+y23)] y23 < ycut) &&
         y13 + y23 <= 1,
        {{y13, 0, 1}, {y23, 0, 1}} ]]

    y23DurhamThreeJetRegionY[ycut_] := Module[
      {y12, y13, y23},
      y12 = 1 - y13 - y23;
      ImplicitRegion[
        (Min[(y12+y13)/(y12+y23), (y12+y23)/(y12+y13)] y12 > ycut &&
         Min[(y12+y13)/(y13+y23), (y13+y23)/(y12+y13)] y13 > ycut &&
         Min[(y12+y23)/(y13+y23), (y13+y23)/(y12+y23)] y23 > ycut),
        {{y13, 0, 1}, {y23, 0, 1}} ]]

  End[]

EndPackage[]