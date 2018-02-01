(* coqdoc --pdf irods_v5_api_rfc.v  -o irods_v5_api_rfc.pdf --preamble "\usepackage{tikz}" *)

(** * Introduction

** System state vs observed state

Some times iRODS needs to perform multiple actions that modifies it state in a transactional fashion. For example, uploading a file and register in iCAT. The state of the system is modified in each action, but only the completed state is observed by client. When the transaction is partially done, the system state encompasses both the observed state and unobserved state. In the Constraints section, we describe each constraint either on the system state or the observed state.

*)

Parameter system_state : Set.
Parameter observed_state : Set.

(**

** Local State vs Global State
For simplicity, we assume that at each moment, a global state encompassing the entire iRODS grid can be defined. In future iterations, it would be interesting to explore models where local states are defined but not global state.

** Identity of an Object
There are two possible way to identify an object, by its content and its relation to other objects, or by an associated id. The former makes the content and relation of an object immutable, such as Merkle trees. In git for example, an identity of a commit is the former. In the this approach, the object is the id. The latter allows mutability but do not have a guarantee of the association of content, relation, and identity. In this approach, the id is the object. iRODS uses the second approach. 

** Sense vs Reference

In describing constraints and actions, we use names to refer to objects. We need to distinguish two uses of terms a la Frege. Sense: a path to a data object is a sense, where as the data object itself is a reference. Therefore, when we say data object a, we mean the id "a", and "a" is a reference to the data object, regardless of the state of the system, whereas, when we say data object with path "a", "a" is a sense, which data object it denotes depends of the state of the system.

Notationally, when we write

[id_of_data_object(b, a)]

We say that [b] is the id of [a], semantically, this is the same as 

[b = a]

whereas

[path_of_data_object(b, a)] 

says that [b] is the path of [a].

* iRODS Data Model

** Sets

Sets defines concepts iRODS is built on.
*)

(** *** Immutable Sets *)
(** Some sets contain immutable objects. *)


Parameter immutable_objects : Set.
Parameter id : Set.
Parameter resc_name : Set.
Parameter data_object_name : Set.
Parameter file_name : Set.
Parameter zone_name : Set.
Parameter user_name : Set.
Parameter group_name : Set.
Parameter rule_name : Set.
Parameter microservice_name : Set.
Parameter API_name : Set.
Parameter path : Set.
Parameter physical_path : Set.
Parameter offset : Set.
Parameter length : Set.
Parameter buffer : Set.
Parameter chksum : Set.
Parameter error : Set.
Parameter replica_content : Set.
Parameter access : Set.
Parameter iCAT : Set.
Parameter host : Set.
Parameter port : Set.
Parameter config : Set.
Parameter AVU : Set.
(** [local_id] is an id that cannot be interpreted by an hosts other than the localhost. This could be for example a pointer. *)

Parameter local_id : Set.
Parameter empty_content : replica_content.
Parameter own : access.

(** *** Mutable Sets *)
(** Some sets contain mutable objects.  *)

Inductive mutable_objects :=
  | data_object_object : mutable_objects
  | collection_object : mutable_objects
  | resource_object : mutable_objects
  | replica_object : mutable_objects
  | user_object : mutable_objects
  | group_object : mutable_objects
  | zone_object : mutable_objects
  | metadata_object : mutable_objects
  | rule_object : mutable_objects
  | microservice_object : mutable_objects
  | PEP_object : mutable_objects
  | API_object : mutable_objects
  | connection_object : mutable_objects
  | data_object_descriptor_object : mutable_objects.


Definition objects : Set := immutable_objects + mutable_objects.

(** [el] returns the set of an object. *)

Parameter el : objects -> Set.

(** [identifier] returns the identifer type of an object. The identifier of an immutable object is its content. A mutable object has an identifier. *)

Parameter identifier : objects -> Set.

Definition identifier_impl (o : objects) : Set :=
  match o with
  | inl io => el o
  | inr mo =>
    match mo with
      | data_object_object => zone_name * id
      | collection_object => zone_name * id
      | resource_object => zone_name * id
  (** replica is identified by a zone, a resource, and a path on that resource *)

      | replica_object => zone_name * id * physical_path
  (** user or group is identified by a zone and a user name in that zone *)

      | user_object => zone_name * user_name
      | group_object => zone_name * group_name
      | zone_object => zone_name
  (** metadata is identified by AVU *)

      | metadata_object => AVU
      | rule_object => host * rule_name
      | microservice_object => host * microservice_name
      | PEP_object => host * rule_name
      | API_object => host * id
      | connection_object => host * local_id
      | data_object_descriptor_object => host * local_id
  end
end.


Definition data_object := el (inr data_object_object).
Definition collection := el (inr collection_object).
Definition resource := el (inr resource_object).
Definition replica := el (inr replica_object).
Definition user := el (inr user_object).
Definition group := el (inr group_object).
Definition zone := el (inr zone_object).
Definition metadata := el (inr metadata_object).
Definition rule := el (inr rule_object).
Definition microservice := el (inr microservice_object).
Definition PEP := el (inr PEP_object).
Definition API := el (inr API_object).
Definition connection := el (inr connection_object).
Definition data_object_descriptor := el (inr data_object_descriptor_object).

(** ** Relations
*)

Parameter relation : Set.
Parameter is_system : relation -> system_state -> Prop.
Parameter is_observed : relation -> observed_state -> Prop.

(** *** List of Relations *)
Parameter data_object_child_of_collection : data_object -> collection -> relation.
Parameter collection_child_of_collection : collection -> collection -> relation.
Parameter collection_root : collection -> relation.
Parameter replica_of : replica -> data_object -> relation.
Parameter stored_at : replica -> resource -> relation.
Parameter resource_child_of_resource : resource -> resource -> relation.
Parameter resource_root : resource -> relation.
Parameter resource_local_to_zone : resource -> zone -> relation.
Parameter replica_local_to_zone : user -> zone -> relation.
Parameter user_has_access_to_data_object : user -> access -> data_object -> relation.
Parameter user_has_access_to_collection : user -> access -> collection -> relation.
Parameter data_object_has_owner: data_object -> user -> relation.
Parameter path_of_data_object : path -> data_object -> relation.
Parameter id_of_data_object : id -> data_object -> relation.
Parameter owner_of_data_object : user -> data_object -> relation.
Parameter content_of_replica : replica_content -> replica -> relation.

Parameter path_of_collection : path -> collection -> relation.
Parameter id_of_collection : id -> collection -> relation.

(** ** Constraints *)

(** *** List of Constraints *)
Hypothesis data_object_is_child_of_a_collection:
forall (s : observed_state) (a : data_object), 
exists (b : collection), is_observed (data_object_child_of_collection a b) s.

Hypothesis data_object_has_at_least_one_replica:
forall (s : observed_state) (a : data_object), 
exists (b : replica), is_observed (replica_of b a) s.

Hypothesis collection_is_child_of_a_collection_or_root:
forall (s : observed_state) (a : collection), 
exists (b : collection), is_observed (collection_child_of_collection a b) s \/ is_observed (collection_root a) s.

(**
 * iRODS Interaction Model *)

Parameter action : Set -> Set.
Parameter query : Set -> Set.
Parameter aux : Set -> Set.


(** ** Actions

Actions can be applied. An applied action has the following type *)

Definition applied_action a := system_state -> (a + error) * system_state.

Parameter apply_action : forall {a : Set}, action a -> applied_action a.

(**

An applied action produces a result [a], can modify the system_state, and can throw an error. 

*** List of Actions *)
Parameter set : relation -> action unit.
Parameter reset : relation -> action unit.
Parameter new_id : action id.
Parameter new_data_object : action data_object.
Parameter new_replica : resource -> path -> action replica.
Parameter data_object_copy : data_object -> path -> connection -> action unit.
Parameter data_object_chksum : data_object -> connection -> action chksum.
Parameter data_object_rename : data_object -> path -> connection -> action unit.
Parameter data_object_phymv : data_object -> physical_path -> connection -> action unit.
Parameter data_object_lock : data_object -> connection -> action unit.
Parameter data_object_unlock : data_object -> connection -> action unit.
Parameter data_object_create : resource -> path -> connection -> action unit.
Parameter data_object_delete : data_object -> connection -> action unit.
Parameter data_object_unlink : data_object -> path -> connection -> action unit.
Parameter data_object_open : data_object -> connection -> action data_object_descriptor.
Parameter data_object_lseek : data_object_descriptor -> offset -> connection -> action unit.
Parameter data_object_close : data_object_descriptor -> connection -> action unit.
Parameter data_object_read : data_object_descriptor -> offset -> length -> connection -> action buffer.
Parameter data_object_write : data_object_descriptor -> offset -> buffer -> connection -> action unit.
Parameter data_object_replicate : user -> data_object -> resource -> connection -> action unit.
Parameter data_object_trim : data_object -> resource -> connection -> action unit.
Parameter data_object_truncate : data_object -> length -> connection -> action unit.
Parameter data_object_rsync : data_object -> data_object -> connection -> action unit.
Parameter data_object_get : action unit.
Parameter data_object_put : action unit.

(**
 ** Queries
Queries can be applied. An applied query has the following type
*)

Definition applied_query a := system_state -> a + error.

(**
An applied query produces a result [a] which may depend on the system state, does not modify the system_state, and can throw an error.

*** List of Queries
 *)

Parameter path_to_data_object : path -> query data_object.
Parameter path_to_collection : path -> query collection.
Parameter connection_user : connection -> query user.
Parameter lift_query : forall {a : Set}, query a -> action a.

(**
 ** Auxiliary Function 

Auxiliary functions can be applied. An applied auxiliary function has the following type
*)
Definition applied_aux (a : Set) : Set := a + error.
(**
An applied auxiliary function produces a result [a] which does not depend on the system state, does not modify the system_state, and can throw an error.

*** List of auxiliary functions
*)
Parameter parent_path : path -> aux path.
Parameter lift_aux : forall {a : Set}, aux a -> action a.

(** ** DAGs
Actions are DAG nodes.
*)

Parameter pure : forall {a : Set}, a -> action a.
Parameter bind : forall {a b : Set}, (a -> action b) -> action a -> action b.
Notation "a >>= b" := (bind b a) (at level 10).
Definition set1 {a: Set} (r1 : a -> relation) (acta : action a) : action unit :=
  acta >>= fun a0 => set (r1 a0).
Definition set2 {a b: Set} (r2 : a -> b -> relation) (acta : action a) (actb : action b) : action unit :=
  acta >>= fun a0 => actb >>= fun b0 => set (r2 a0 b0).
Definition set3 {a b c: Set} (r3 : a -> b -> c -> relation) (acta : action a) (actb : action b) (actc : action c) : action unit :=
  acta >>= fun a0 => actb >>= fun b0 => actc >>= fun c0 => set (r3 a0 b0 c0).

Definition lift_query1 {a b: Set} (q1 : a -> query b) (acta : action a) : action b :=
  acta >>= fun a0 => lift_query (q1 a0).

Parameter parallel : forall {a : Set}, action a -> action a -> action a.
Notation "a | b" := (parallel a b) (at level 11).


Definition data_object_create_impl (r : resource) (p : path) (c : connection) :=
  let u := lift_query (connection_user c) in
  let di := new_id in
  let d := new_data_object in
  let rep := new_replica r p in
  let pp := lift_aux (parent_path p) in
  let col := lift_query1 path_to_collection pp in
  set2 id_of_data_object di d |
  set2 replica_of rep d |
  set2 content_of_replica (pure empty_content) rep |
  set2 owner_of_data_object u d |
  set3 user_has_access_to_data_object u (pure own) d |
  set2 path_of_data_object (pure p) d |
  set2 data_object_child_of_collection d col.



(**
data_object_create r p c =
%
\begin{tikzpicture}[scale=.5, every node/.style={scale=0.5,draw, rectangle}]
\node (u) at (0,0) {u := connection\_user c};
\node (di) at (5,0) {di := new\_id};
\node (d) at (10,0) {d := new\_data\_object};
\node (rep) at (15,0) {rep := new\_replica r p};
\node (pp) at (22.5, 0) {pp := parent\_path p};
\node (set_id) at (0,-3) {set (id\_of\_data\_object di d)};
\node (set_replica) at (7.5,-3) {set (replica\_of rep d)};
\node (set_content) at (15,-3) {set (content\_of\_replica empty\_content rep)};
\node (col) at (22.5,-3) {col := path\_to\_collection pp};
\node (set_owner) at (0,-6) {set (owner\_of\_data\_object u d)};
\node (set_access) at (7.5,-6) {set (user\_has\_access\_to\_data\_object u own d)};
\node (set_path) at (15,-6) {set (path\_of\_data\_object p d)};
\node (set_col) at (22.5,-6) {set (collection\_of\_data\_object col d)};

\draw [->] (u) -- (set_owner);
\draw [->] (u) -- (set_access);
\draw [->] (di) -- (set_id);
\draw [->] (d) -- (set_id);
\draw [->] (d) -- (set_replica);
\draw [->] (d) -- (set_owner);
\draw [->] (d) -- (set_access);
\draw [->] (d) -- (set_path);
\draw [->] (d) -- (set_col);
\draw [->] (pp) -- (col);
\draw [->] (col) -- (set_col);
\draw [->] (rep) -- (set_replica);
\draw [->] (rep) -- (set_content);


\end{tikzpicture}
%


** Constraints
*)
Hypothesis id_of_data_object_immutability:
forall (s s' : system_state) (a : Set) (act : action a),
snd (apply_action act s) = s' -> 
forall (i : id) (a b : data_object), 
is_system (id_of_data_object i a) s /\ is_system (id_of_data_object i b) s' ->
a = b.
