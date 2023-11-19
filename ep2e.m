(* Wolfram Language Tools *)

    (* Formatting numbers *)
    fN[x_] := If[x == Round[x], Round[x], N[x]];
    fN[x_, digits : _Integer] :=  If[x == Round[x], Round[x], N[x, digits]];
    fN[x_, precision_] :=  If[Abs[x - Round[x]] <= precision, Round[x], N[x]];
    fN[x_, precision_, digits : Integer] := 
        If[Abs[x - Round[x]] <= precision, Round[x], N[x, digits]];

(* Stochastic Functions *)

    (* Probability to roll exactly a sum of p with n s-sided dice. *)
    diceP[p_, n_, s_] := Times[Power[s,Times[-1,n]],Sum[Times[Power[-1,k],Binomial[n,k],Binomial[Plus[-1,p,Times[-1,k,s]],Plus[Times[-1,n],p,Times[-1,k,s]]]],List[k,0,Floor[Times[Plus[Times[-1,n],p],Power[s,-1]]]]]];

    (* Probability to roll at least a sum of minSum with n s-sided dice. *)
    diceSumP[minSum_, n_, s_] := Sum[Times[Power[s,Times[-1,n]],Sum[Times[Power[-1,k],Binomial[n,k],Binomial[Plus[-1,i,Times[-1,k,s]],Plus[i,Times[-1,n],Times[-1,k,s]]]],List[k,0,Floor[Times[Plus[i,Times[-1,n]],Power[s,-1]]]]]],List[i,minSum,Times[n,s]]];

    (* Probability to get exactly k successes in n experiments/trials with individual Probability of success per experiment/trial of p. *)
    pKSuccessesInNTrials[k_, n_, p_] := Times[Power[Plus[1,Times[-1,p]],Plus[Times[-1,k],n]],Power[p,k],Binomial[n,k]];

    (* Probability to get at least k successes in n experiments/trials for a probability to succeed in any individual experiment/trial of p. *)
    pAtLeastKSuccessesInNTrials[k_, n_, p_] := Plus[1,Times[-1,Power[Plus[1,Times[-1,p]],n],Plus[Power[Power[Plus[1,Times[-1,p]],-1],n],Times[-1,Power[Plus[1,Times[-1,p]],Times[-1,k]],Power[p,k],Binomial[n,k],Hypergeometric2F1[1,Plus[k,Times[-1,n]],Plus[1,k],Times[Power[Plus[-1,p],-1],p]]]]]];

    (* Average roll of a n-sided dice. *)
    d[n_] := (n+1)/2;

    (* rolling n dN-sided dice *)
    roll[n_, dN_] := Table[RandomInteger[{1, dN}], n];


(* Physics *)

    gEarth = Quantity[9.8,"Meters"]/Times[Quantity[1,"Seconds"],Quantity[1,"Seconds"]];
    (*leakPressureLoss[volume_,leakSurfaceArea_,time_]:=*)

(*
With[{date = DateObject[{2023, 12, 1}],vP=Times[1/10,Quantity[1,"SpeedOfLight"]]}, distance = AstroDistance[Entity["Planet", "Jupiter"], {Entity["Planet", "Earth"], date}]; {{"time[C]", UnitConvert[distance/Quantity[1, "SpeedOfLight"], MixedUnit[{"Days", "Hours", "Minutes", "Seconds"}]]},{"time[vPhysical]",UnitConvert[distance/vP, MixedUnit[{"Days", "Hours", "Minutes", "Seconds"}]]}}]

With[{date = DateObject[{2023, 12, 1}],vP=Quantity[17000,"Meters"]/Quantity[1,"Seconds"]}, distance = AstroDistance[Entity["Planet", "Jupiter"], {Entity["Planet", "Earth"], date}]; {{"time[C]", UnitConvert[distance/Quantity[1, "SpeedOfLight"], MixedUnit[{"Days", "Hours", "Minutes", "Seconds"}]]},{StringJoin[{"time[",ToString[vP],"]"}],UnitConvert[distance/vP, MixedUnit[{"Days", "Hours", "Minutes", "Seconds"}]]}}]
*)

(* EGR Functions *)
    egrName[n_] := StringJoin[{"EGR_", ToString[N[E, n]]}]; (* name of EGR version n, with versions lesser than 3 making no sense, and lesser than 6 only very little as they are retired to archive *)
    egrFork[source_, n_] := StringJoin[{source, ".", ToString[n]}]; (* source name and nth fork from this source*)

Print["ep2e.m loaded"];