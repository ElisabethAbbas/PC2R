(*ocamlc -o serv.exe -thread -custom str.cma unix.cma threads.cma server.ml -cclib -lthreads -cclib -lunix 
emacs : set compiler flags*)

let tps_dmr = 30.;;
let server_tickrate = 60.;;
let server_refresh_tickrate = 100.;;
let turnit=15;;
let thrustit=1.8;;(*0.01;;*)
let fin_session = ref false;;
let ob_radius = 10.0;;
let ve_radius = 15.0;;

let random_coord() =
  print_endline "20";
  30.+.Random.float(600.-.60.), 30.+.Random.float(600.-.60.);

class virtual server port n =
        object (s)
          val port_num = port
          val nb_pending = n
          val sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0
          method start () =
            let host = Unix.gethostbyname (Unix.gethostname()) in
            let h_addr = host.Unix.h_addr_list.(0) in
            let sock_addr = Unix.ADDR_INET(h_addr, port_num) in
            Unix.bind sock sock_addr ;
            Unix.listen sock nb_pending ;
            while true do
              let (service_sock, client_sock_addr) =
                Unix.accept sock
              in
              s#treat service_sock client_sock_addr
            done
          method virtual treat : Unix.file_descr -> Unix.sockaddr -> unit
        end ;;

let gen_num = let c = ref 0 in (fun () -> incr c; !c) ;;

let outchans:((int, out_channel) Hashtbl.t) = Hashtbl.create 50
let inchans:((int, in_channel) Hashtbl.t) = Hashtbl.create 50
let joueurs:((int, string) Hashtbl.t) = Hashtbl.create 50
let numeros:((string, int) Hashtbl.t) = Hashtbl.create 50
let coordonnees:((string, (float*float)) Hashtbl.t) = Hashtbl.create 50
let vcoordonnees:((string, (float*float*float*float*float)) Hashtbl.t) = Hashtbl.create 50
let obstacles:(float*float) list ref = ref [] 
let coordonnee_obj:(float*float) ref = ref (0.,0.) 
let scorestable:((string, int) Hashtbl.t) = Hashtbl.create 50
let phase = ref ""
          
let coordonnees_to_coords () =
  print_endline "1";
  let coords = ref "" in 
  Hashtbl.iter (fun a (b1,b2) -> coords:=a^":"^"X"^(string_of_float b1)^"Y"^(string_of_float b2)^"|"^(!coords)) coordonnees;
  if (String.length !coords) = 0
  then
    !coords
  else String.sub !coords 0 ((String.length !coords)-1) ;;

let obstacles_to_ocoords () =
  print_endline "24";
  let ocoords = ref "" in 
  List.iter (fun (cx, cy) -> ocoords:="X"^(string_of_float cx)^"Y"^(string_of_float cy)^"|"^(!ocoords)) !obstacles;
  if (String.length !ocoords) = 0
  then
    !ocoords
  else String.sub !ocoords 0 ((String.length !ocoords)-1) ;;


let vcoordonnees_to_vcoords () =
  print_endline "22";
  let vcoords = ref "" in 
  Hashtbl.iter (fun a (bx,by,bvx,bvy,bt) -> vcoords:=a^":"^"X"^(string_of_float bx)^"Y"^(string_of_float by)^"VX"^(string_of_float bvx)^"VY"^(string_of_float bvy)^"T"^(string_of_float bt)^"|"^(!vcoords)) vcoordonnees;
  if (String.length !vcoords) = 0
  then
    !vcoords
  else String.sub !vcoords 0 ((String.length !vcoords)-1) ;;
;;

let coordonnee_to_coord c =
  print_endline "2";
  match c with
  | (f1, f2) -> "X"^string_of_float(f1)^"Y"^string_of_float(f2)
;;

let scorestable_to_scores () =
  print_endline "3";
  let scores = ref "" in 
  Hashtbl.iter (fun a sc -> scores:=a^":"^(string_of_int sc)^"|"^(!scores)) scorestable;
  if (String.length !scores) = 0
  then !scores
  else String.sub !scores 0 ((String.length !scores)-1) ;;
;;

class virtual connexion sd (sa : Unix.sockaddr) b =
        object (self)
          val s_descr = sd
          val outchan =  Unix.out_channel_of_descr sd
          val inchan =  Unix.in_channel_of_descr sd
          val s_addr = sa
          val mutable numero = 0
          val mutable debug = b
          val mutable temps_premiere_connexion = 0.
          val mutable scores = []
          val mutable continue = ref true;
                                 
          method set_debug b = debug <- b
          initializer
            numero <- gen_num();
            if debug then
              begin
                Printf.printf "TRACE.connexion : objet traitant %d cree\n" numero ;
                print_newline()
              end;
            if numero=1 then
              begin
                coordonnee_obj:=random_coord();
                temps_premiere_connexion <- Sys.time();
              end;
          method start () = Thread.create (fun x -> self#run x ; self#stop x) ()
          method stop() =
            if debug then
              begin
                Printf.printf "TRACE.connexion : fin objet traitant %d\n" numero ;
                print_newline ()
              end ;
            Unix.close s_descr
          method virtual run : unit -> unit
          method getnum() = numero
          method getinchan : unit -> in_channel = fun () -> inchan
          method getoutchan() = outchan
          method getcontinue() = continue
          method setcontinue(c:bool) = (continue:=c)
        end;;

exception Fin ;;

let command_welcome s =
  print_endline "4";
  let bienvenue = "WELCOME/"^(!phase)^"/"^(scorestable_to_scores())^"/"^(coordonnee_to_coord(!coordonnee_obj))^"/"^(obstacles_to_ocoords())^"/\n" in 
  output (s#getoutchan()) bienvenue 0 (String.length bienvenue); flush (s#getoutchan());;

let command_denied s =
  print_endline "5";
  let refuse = "DENIED/\n" in 
  output (s#getoutchan()) refuse 0 (String.length refuse); flush (s#getoutchan());;

let command_newplayer s pseudo =
  print_endline "6";
  let nvj = "NEWPLAYER/"^pseudo^"/\n" in
  Hashtbl.iter (fun a b ->
      begin
        if (s#getnum())!=a
        then
          output b nvj 0 (String.length nvj); flush b
      end
    ) outchans;
;;

let commande_tick () =
  print_endline "7";
  let vcoords = vcoordonnees_to_vcoords() in
  print_endline vcoords;
  let tick = "TICK/"^vcoords^"/\n" in
  Hashtbl.iter (fun a b -> (output b tick 0 (String.length(tick)); flush b)) outchans;;

(*let m_tick = Mutex.create();;
let c_tick = Condition.create();;
let newpos_done = 1;;*)

(*  expliquer dans rapport que pour faire une scheduled task en ocaml 
on fait une promesse, ou alors fairthreads mais expliquer que ja'ai pu pu installer
http://ocsigen.org/lwt/4.1.0/manual/manual *)
let rec serveur_tickrate_task () =
  print_endline "8";
  Thread.delay (1./.server_refresh_tickrate); (*Thred.delay : les autres thead travaille, mais manque de précision sur les ticks, ils peuvent être plus longs vu que pas coopératif*)
  (*on change pour plus de précision avec un sigalarm :
cristal.inria.fr/~remy/poly/system/camlunix/sign.html*)
  commande_tick();

  (*Mutex.lock m_tick; (*EXPLIQUER dans rapport*)
  while newpos_done!=1
  do Condition.wait c_tick m_tick
  done;
  Mutex.unlock m_tick;*)
  if(not !fin_session) then
    serveur_tickrate_task ();
;;


let commande_session s =
  print_endline "9";
  phase:="jeu";
  let coords = coordonnees_to_coords() in 
  let coord = coordonnee_to_coord(!coordonnee_obj) in
  let obst =  obstacles_to_ocoords() in
  let session = "SESSION/"^coords^"/"^coord^"/"^obst^"/\n" in
  Hashtbl.iter (fun a b -> (output b session 0 (String.length session); flush b)) outchans ;
  Thread.create serveur_tickrate_task()
;; 

let commande_connexion cmd s =
  print_endline "11";
  begin
    print_endline ("Nouvelle connexion d’un client nomme '"^(List.nth cmd 1)^"'");
    if (not (Hashtbl.mem numeros (List.nth cmd 1)))
    then 
      begin
        command_newplayer s (List.nth cmd 1) ;
        Hashtbl.add joueurs (s#getnum()) (List.nth cmd 1) ;
        Hashtbl.add numeros (List.nth cmd 1) (s#getnum()) ;
        Hashtbl.add inchans (s#getnum()) (s#getinchan());
        Hashtbl.add outchans (s#getnum()) (s#getoutchan());
        Hashtbl.add coordonnees (List.nth cmd 1) (random_coord());
        let (cx, cy) = random_coord() in
        Hashtbl.add vcoordonnees (List.nth cmd 1) (cx, cy, 0., 0., 0.);
        Hashtbl.add scorestable (List.nth cmd 1) 0;
        command_welcome s
      end
    else
      command_denied s;
  end;;

let commande_playerleft cmd s =
  print_endline "12";
  let plyrleft = "PLAYERLEFT/"^(List.nth cmd 1)^"/\n" in
  Hashtbl.iter (fun a b -> (output b plyrleft 0 (String.length plyrleft); flush b)) outchans

let commande_deconnexion cmd s =
  print_endline "13";
  print_endline ("Deconnexion de "^(List.nth cmd 1));
  Hashtbl.remove joueurs (s#getnum()) ;
  Hashtbl.remove numeros (List.nth cmd 1) ;
  Hashtbl.remove inchans (s#getnum()) ;
  Hashtbl.remove outchans (s#getnum()) ;
  Hashtbl.remove coordonnees (List.nth cmd 1);
  Hashtbl.remove vcoordonnees (List.nth cmd 1);
  Hashtbl.remove scorestable (List.nth cmd 1);
  s#setcontinue(false);
  commande_playerleft cmd s

let commande_newpos (cmd: string list) s =
  print_endline "14";
  let sansX = List.nth (Str.split (Str.regexp "X")(List.nth cmd 1)) 0 in
  let xy = Str.split (Str.regexp "Y") sansX in
  let x = List.nth xy 0 in
  let y = List.nth xy 1 in
  Hashtbl.replace coordonnees (Hashtbl.find joueurs (s#getnum())) (float_of_string x, float_of_string y) ;;

let commande_newobj () =
  print_endline "15";
  coordonnee_obj:=random_coord();
  let newobj = "NEWOBJ/"^(coordonnee_to_coord(!coordonnee_obj))^"/"^scorestable_to_scores()^"/\n" in
  Hashtbl.iter (fun a b -> (output b newobj 0 (String.length(newobj)); flush b)) outchans
;;

let commande_winner s =
  print_endline "10";
  let pseudo = Hashtbl.find joueurs (s#getnum()) in
  let score = Hashtbl.find scorestable pseudo in
  Hashtbl.replace scorestable pseudo (score+1);
  let winners = "WINNER/"^(scorestable_to_scores())^"/\n" in
  fin_session:=true;
  Hashtbl.iter (fun a b -> (output b winners 0 (String.length winners); flush b)) outchans ;;

let commande_newcom (cmd: string list) s =
  print_endline "21";
  let sansA = List.nth (Str.split (Str.regexp "A")(List.nth cmd 1)) 0 in
  let at = Str.split (Str.regexp "T") sansA in
  let a = float_of_string (List.nth at 0) in
  let t = int_of_string (List.nth at 1) in
  let pseudo = Hashtbl.find joueurs (s#getnum()) in
  let (x, y, vx, vy, angle) = Hashtbl.find vcoordonnees pseudo in
  let newx = ref x in
  let newy = ref y in 
  let newvx = ref vx in
  let newvy = ref vy in
  let newa = ref (a+.angle) in
  if t=0
  then
    begin
      newx:=!newx+.(!newvx);
      if !newx>600. then
        newx:=!newx-.600.;
      if !newx<0. then
        newx:=!newx+.600.;
      newy := !newy+.(!newvy);
      if !newy>600. then
        newy:=!newy-.600.;
      if !newy<0. then
        newy:=!newy+.600.;
    end
  else
    for i=1 to t do
      newvx := !newvx +. thrustit*.cos(!newa*.3.1416/.180.0);
      newvy := !newvy +. thrustit*.sin(!newa*.3.1416/.180.0);
      newx:=!newx+.(!newvx);
      if !newx>600. then
        newx:=!newx-.600.;
      if !newx<0. then
        newx:=!newx+.600.;
      newy := !newy+.(!newvy);
      if !newy>600. then
        newy:=!newy-.600.;
      if !newy<0. then
        newy:=!newy+.600.;
    done;
  if Hashtbl.mem vcoordonnees pseudo
  then
    Hashtbl.replace vcoordonnees pseudo (!newx, !newy, !newvx, !newvy, !newa) ;
  match !coordonnee_obj with
    (xo, yo) ->
    begin
      let d2 = (xo-.(!newx))*.(xo-.(!newx)) +. (yo-.(!newy))*.(yo-.(!newy)) in
      if (d2 <= (ob_radius +. ve_radius)*.(ob_radius +. ve_radius))
      then commande_winner s
    end;
    Hashtbl.iter (fun ps (x,y,vx,vy,t) ->
        begin
          if (not (String.equal ps pseudo)) then
            begin
              let d2 = (!newx-.x)*.(!newx-.x) +. (!newy-.y)*.(!newy-.y) in
              if (d2 <= (ve_radius +. ve_radius)*.(ve_radius +. ve_radius))
              then
                begin
                  Hashtbl.replace vcoordonnees pseudo (!newx, !newy, 0.-.(!newvx), 0.-.(!newvy), !newa) ;
                  Hashtbl.replace vcoordonnees ps (x, y, 0.-.vx, 0.-.vy, t) ;
                end;
            end;
        end)
      vcoordonnees;
;;

let lecture s  =
  begin
    try
      while true do
        if !fin_session then
          raise Fin;
        print_endline "16";
        (* commencer partie *)
        let ligne = input_line (s#getinchan())
        in if (ligne = "") || (ligne = "\013")
           then raise Fin ;
           let cmd = Str.split (Str.regexp "/") ligne in
           match (List.nth cmd 0) with
           | "CONNECT" -> commande_connexion cmd s
           | "EXIT" -> commande_deconnexion cmd s; raise Fin 
           | "NEWPOS" -> commande_newpos cmd s                    
           | "NEWCOM" -> commande_newcom cmd s
           | _ -> print_endline "commande inconnue"
      done
    with
      Fin -> () (*commande_deconnexion ["EXIT"; (Hashtbl.find joueurs (s#getnum()))] s*)
    | exn -> print_string (Printexc.to_string exn) ; print_newline()
  end;;
(* pour démarrer une session, envoie "en même temps" sur tous les outchans pour que le jeu démarre en même temps*) 


(*let de_attente_a_jeu _ =
  commande_session()*)

(*On explique pq on change ; erreur : Fatal error: exception Unix.Unix_error(Unix.EINTR, "accept", "")*)

(*let attente_debut_session s =
  if !phase!="attente"
  then ignore(Sys.signal Sys.sigalrm (Sys.Signal_handle(de_attente_a_jeu))); ignore(Unix.alarm tps_dmr);
  phase:="attente";; 
 *)


let m_session = Mutex.create();;
let c_session = Condition.create();;


let attente_debut_session s =
  print_endline "17";
  Mutex.unlock m_session;
  Thread.delay(tps_dmr);
  
  commande_session()
;;

let nettoyer_variables_globales() =
  Hashtbl.iter (
      fun pseudo (x, y, vx, vy, t)->
      match random_coord() with
        (rx, ry) ->
        (Hashtbl.replace vcoordonnees pseudo (rx, ry, 0., 0., 0.))) vcoordonnees; coordonnee_obj:= random_coord();
  obstacles:=[random_coord(); random_coord(); random_coord()]
;;

(*mettre mutex sur les variables partagées !! et mettre dans rapport*)
(*mettre dans rapport où j'ai créé des threads.*)
(*expliquer pourquoi j'ai créé joueurs_session, pour synchroniser les threads*)
let (joueurs_session: int Stack.t) = Stack.create()
                                   
class connexion_maj sd sa b =
object(self)
  inherit connexion sd sa b as super
  method run () =
    while !continue do
      begin
        print_endline "18";
        (*mettre sigalarm plutôt + dire ds rapport*) 
        begin
          Mutex.lock m_session;
          if not (String.equal !phase "attente")
          then
            begin
              phase:="attente";
              ignore(Thread.create attente_debut_session self);
              nettoyer_variables_globales();
              commande_newobj ();
            end;
          Stack.push (self#getnum()) joueurs_session;
          (* expliquer dans rapport. pour synchro les threads *)
          while (Stack.length joueurs_session < Hashtbl.length joueurs)
          do Condition.wait c_session m_session
          done;
          (*si on n'attend plus personne.*)
          (* >= au cas où quelqu'un deco*)
          if (Stack.length joueurs_session >= Hashtbl.length joueurs)
          then
            begin
              Stack.clear joueurs_session;
              Condition.broadcast c_session
            end;
          Mutex.unlock m_session
        end;
        fin_session:=false;
        let lecteur = (Thread.create lecture self)
        in
        Thread.join lecteur;
      end;
    done
end ;;

class server_maj port n =
object(s)
  inherit server port n
  method treat s sa =
    print_endline "19";
    ignore( (new connexion_maj s sa true)#start());
end;;
(*
val main : unit -> unit
 *)
let main () =
  if Array.length Sys.argv < 3
  then Printf.printf "usage : server port num\n"
  else
    Random.self_init();
  let port = int_of_string(Sys.argv.(1))
  and n = int_of_string(Sys.argv.(2)) in
  (new server_maj port n )#start();;
main();;
