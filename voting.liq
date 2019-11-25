type storage = { candidates : (string, nat) map; (**Political candidates*)
                 voters : (address, bool) map;(** Used to register voters *) 
                 voting_ended: bool; (** Manually close voting if all voters have votes *) 
                 deadline : timestamp; (** Automatic Deadline after which vote closes *) }

let%init init_candidates (candidate_names : string list) =
  let candidates = List.fold (fun (elt, map) -> Map.add elt 0p map) candidate_names 
      (Map [] : (string, nat) map) in
  { candidates = candidates; voters = (Map : (address, bool) map); voting_ended = false;deadline = Current.time () + 3600 * 24 (* 1 day from now *) }
 

let%entry add_candidate
    (candidate : string)
    (storage : storage) =
  
  if Map.size storage.voters > 0p then
    failwith "Voting has already begun";
  
  (* Only allowed while voting period is ongoing *)
  if Current.time () > storage.deadline then failwith "Voting closed";

  if storage.voting_ended = true then
    Current.failwith ("Voting has already ended");

  let storage = 
    storage.candidates <- match Map.find candidate storage.candidates with 
      | None -> Map.add candidate 0p storage.candidates
      | Some x -> failwith ("Candidate already present", x)
  in
  ( ([]: operation list), storage )

     
let%entry remove_candidate
    (candidate : string)
    (storage : storage) =
  
  if Map.size storage.voters > 0p then
    failwith "Voting has already begun";

  if storage.voting_ended = true then
    Current.failwith ("Voting has already ended");
  
  (* Only allowed while voting period is ongoing *)
  if Current.time () > storage.deadline then failwith "Voting closed";

  let storage = 
    storage.candidates <- match Map.find candidate storage.candidates with 
      | None -> Map.remove candidate storage.candidates
      | Some x -> failwith ("Candidate already present", x)
  in
  ( ([]: operation list), storage )

  
let%entry close_election
    (parameter : unit)
    (storage : storage) = 
  let storage = storage.voting_ended <- true in
  ( ([]: operation list), storage )

  (* Auxiliary function : returns the list of candidates with the
   maximum number of votes (there can be more than one in case of
   draw). *)
let find_winners candidates =
  let winners, _max =
    Map.fold (fun ((name, nb), (winners, max)) ->
        if nb = max then
          name :: winners, max
        else if nb > max then
          [name], nb
        else winners, max
      ) candidates ([], 0p) in
  winners  
  
(**Voting a candidate*)
let%entry main
    (parameter : string)
    (storage : storage) =

  if storage.voting_ended = true then
    Current.failwith ("Voting has already ended"); 
  
  (* Only allowed while voting period is ongoing *)
  if Current.time () > storage.deadline then failwith "Voting closed";
  let winners = find_winners storage.candidates  in
  
  let addr = Current.source () in
  let storage = 
    storage.voters <- match Map.find addr storage.voters with
      | None -> Map.add addr true storage.voters
      | Some x -> failwith ("Voter has already voted", addr)
  in
  
  let storage =
    storage.candidates <- match Map.find parameter storage.candidates with
      | None -> failwith ("Candidate is not valid", parameter)
      | Some x -> Map.add parameter (x + 1p) storage.candidates
  in

  ( ([]: operation list), storage )
