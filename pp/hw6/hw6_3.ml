exception TODO

type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
          | Poland | Portugal | Italy | Germany | Norway | Sweden | England
          | Argentina

type tourna = LEAF of team
            | NODE of tourna * tourna

let string_of_team (t: team) =
  match t with
  | Korea -> "Korea"
  | France -> "France"
  | Usa -> "Usa"
  | Brazil -> "Brazil"
  | Japan -> "Japan"
  | Nigeria ->"Nigeria"
  | Cameroon ->"Cameroon"
  | Poland -> "Poland"
  | Portugal -> "Portugal"
  | Italy -> "Italy"
  | Germany -> "Germany"
  | Norway -> "Norway"
  | Sweden -> "Sweden"
  | England -> "England"
  | Argentina -> "Argentina"

(*FIXME*)
let rec drop (t: tourna) (d: team): string =
  match t with
  | LEAF n -> 
      if d = n 
      then "" 
      else (string_of_team n)
  | NODE (t_l, t_r) -> 
      if drop t_l d = ""
      then drop t_r d 
      else if drop t_r d = ""
      then drop t_l d 
      else 
        String.concat "" ["("; (drop t_l d); " "; (drop t_r d); ")"]
