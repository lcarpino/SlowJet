(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Jade *)

(* :Author: *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

BeginPackage["SlowJet`EPA`Jade`"]

  y23JadeTwoJetX::usage = "y23JadeTwoJetX[ycut, x1, x2] returns 1 if the point (x1, x2) is inside the two-jet region and 0 if the point (x1, x2) is outside the two-jet region"
  y23JadeThreeJetX::usage = "y23JadeThreeJetX[ycut, x1, x2] returns 1 if the point (x1, x2) is inside the three-jet region and 0 if the point (x1, x2) is outside the three-jet region"

  y23JadeTwoJetRegionX::usage = "y23JadeTwoJetRegionX[ycut] returns an implicit region of the two-jet phase space in terms of x1 and x2"
  y23JadeThreeJetRegionX::usage = "y23JadeThreeJetRegionX[ycut] returns an implicit region of the three-jet phase space in terms of x1 and x2"


  y23JadeTwoJetY::usage = "y23JadeTwoJetY[ycut, y13, y23] returns 1 if the point (y13, y23) is inside the two-jet region and 0 if the point (y13, y23) is outside the two-jet region"
  y23JadeThreeJetY::usage = "y23JadeThreeJetY[ycut, y13, y23] returns 1 if the point (y13, y23) is inside the three-jet region and 0 if the point (y13, y23) is outside the three-jet region"

  y23JadeTwoJetRegionY::usage = "y23JadeTwoJetRegionY[ycut] returns an implicit region of the two-jet phase space in terms of y13 and y23"
  y23JadeThreeJetRegionY::usage = "y23JadeThreeJetRegionY[ycut] returns an implicit region of the three-jet phase space in terms of y13 and y23"


  Begin["`Private`"]

    (* Jade algorithm in terms of x_i variables *)

    y23JadeTwoJetX[ycut_, x1_, x2_] := Module[
      {x3=2-x1-x2},
      If[ x1 == 1 || x2 == 1 || x3 == 1,
        1,
        Boole[
          ((1 - x3) < ycut ||
           (1 - x2) < ycut ||
           (1 - x1) < ycut) &&
          x1 + x2 >= 1]
        ]]

    y23JadeThreeJetX[ycut_, x1_, x2_] := Module[
      {x3=2-x1-x2},
      (Boole[(1 - x3) > ycut]
       Boole[(1 - x2) > ycut]
       Boole[(1 - x1) > ycut]
       Boole[x1 + x2 >= 1])]

    y23JadeTwoJetRegionX[ycut_] := Module[
      {x1, x2, x3},
      x3 = 2 - x1 - x2;
      ImplicitRegion[
        ((1 - x3) < ycut ||
         (1 - x2) < ycut ||
         (1 - x1) < ycut) &&
        x1 + x2 >= 1,
        {{x1, 0, 1}, {x2, 0, 1}} ]]

    y23JadeThreeJetRegionX[ycut_] := Module[
      {x1, x2, x3},
      x3 = 2 - x1 - x2;
      ImplicitRegion[
        ((1 - x3) > ycut &&
         (1 - x2) > ycut &&
         (1 - x1) > ycut),
        {{x1, 0, 1}, {x2, 0, 1}} ]]


    (* Jade algorithm in terms of y_ij variables *)

    y23JadeTwoJetY[ycut_, y13_, y23_] := Module[
      {y12=1-y13-y23},
      If[ y12 == 1 || y13 == 1 || y23 == 1,
        1,
        Boole[
          (y12 < ycut ||
           y13 < ycut ||
           y23 < ycut) &&
          y13 + y23 <= 1]
        ]]

    y23JadeThreeJetY[ycut_, y13_, y23_] := Module[
      {y12=1-y13-y23},
      (Boole[y12 > ycut]
       Boole[y13 > ycut]
       Boole[y23 > ycut]
       Boole[y13 + y23 <= 1])]

    y23JadeTwoJetRegionY[ycut_] := Module[
      {y12, y13, y23},
      y12 = 1 - y13 - y23;
      ImplicitRegion[
        (y12 < ycut ||
         y13 < ycut ||
         y23 < ycut) &&
         y13 + y23 <= 1,
        {{y13, 0, 1}, {y23, 0, 1}} ]]

    y23JadeThreeJetRegionY[ycut_] := Module[
      {y12, y13, y23},
      y12 = 1 - y13 - y23;
      ImplicitRegion[
        (y12 > ycut &&
         y13 > ycut &&
         y23 > ycut),
        {{y13, 0, 1}, {y23, 0, 1}} ]]

  End[]

EndPackage[]