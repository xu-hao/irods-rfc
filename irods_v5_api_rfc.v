(* coqdoc --pdf irods_v5_api_rfc.v  -o irods_v5_api_rfc.pdf --preamble "\usepackage{tikz}" *)

(** 
* Introduction
*)

Module Type iRODS_model.

(**
** System state vs observed state

Some times iRODS needs to perform multiple actions that modifies it state in a transactional fashion. For example, uploading a file and register in iCAT. The state of the system is modified in each action, but only the completed state is observed by client. When the transaction is partially done, the system state encompasses both the observed state and unobserved state. In the Constraints section, we describe each constraint either on the system state or the observed state.

*)

Parameter system_state : Set.
Parameter observed_state : Set.

Parameter observe : system_state -> observed_state.

(**

** Local State vs Global State
For simplicity, we assume that at each moment, a global state encompassing the entire iRODS grid can be defined. In future iterations, it would be interesting to explore models where local states are defined but not global state.

** Identity of an Object
There are two possible way to identify an object, by its content and its relation to other objects, or by an associated id. The former makes the content and relation of an object immutable, such as Merkle trees. In git for example, an identity of a commit is the former. In the this approach, the object is the id. The latter allows mutability but do not have a guarantee of the association of content, relation, and identity. In this approach, the id is the object.

** Sense vs Reference

In describing constraints and actions, we use names to refer to objects. We need to distinguish two uses of terms a la Frege. Sense: a path to a data object is a sense, where as the data object itself is a reference. Therefore, when we say data object a, we mean the id "a", and "a" is a reference to the data object, regardless of the state of the system, whereas, when we say data object with path "a", "a" is a sense, which data object it denotes depends of the state of the system.

Notationally, when we write

[id_of_data_object(b, a)]

We say that [b] is the id of [a], semantically, this is the same as 

[b = a]

whereas

[path_of_data_object(b, a)] 

says that [b] is the path of [a].

*)

(**
* iRODS Data Model
iRODS data model defines the static aspect of iRODS.
*)


(** 
** Types

Types define concepts iRODS is built on. We define types that are inhabited by certain objects. For example, we define the type [data_object] as being inhabited by data objects.

[objects] is the universe of all names of types. It is universe a la Tarski. For example, we may define the type name [data_object_object] as being the name of type [data_object].
*)

Parameter objects : Set.

(** [el] maps type names to types. *)

Parameter el : objects -> Set.

(** We identify objects by their identifiers. [identifier] returns the identifer type of an object. *)

Parameter identifier_type : objects -> Set.

Parameter identifier : forall {obj : objects}, el obj -> identifier_type obj.

(** The identifier of an object is unique. *)
Axiom identifier_unique:
forall (obj : objects) (a b : el obj),
identifier a = identifier b -> a = b.

(** 
*** List of Types
*)

Parameter id : objects.
Parameter resc_name : objects.
Parameter data_object_name : objects.
Parameter file_name : objects.
Parameter zone_name : objects.
Parameter user_name : objects.
Parameter group_name : objects.
Parameter rule_name : objects.
Parameter microservice_name : objects.
Parameter API_name : objects.
Parameter path : objects.
Parameter physical_path : objects.
Parameter offset : objects.
Parameter length : objects.
Parameter buffer : objects.
Parameter chksum : objects.
Parameter error : objects.
Parameter replica_content : objects.
Parameter access : objects.
Parameter iCAT : objects.
Parameter host : objects.
Parameter port : objects.
Parameter config : objects.
Parameter AVU : objects.
Parameter data_object : objects.
Parameter collection : objects.
Parameter resource : objects.
Parameter replica : objects.
Parameter user : objects.
Parameter group : objects.
Parameter zone : objects.
Parameter metadata : objects.
Parameter rule : objects.
Parameter microservice : objects.
Parameter PEP : objects.
Parameter API : objects.
Parameter connection : objects.
Parameter data_object_descriptor : objects.

(** 
*** List of Constants
*)

Parameter empty_content : el replica_content.
Parameter null read write own : el access.

(** 
** Relations

Objects can form relations.
*)

Parameter relation : Set.

(**
Each relation can be queried in both system states and observed states
*)

Parameter is_system : relation -> system_state -> Prop.
Parameter is_observed : relation -> observed_state -> Prop.

(** 
*** List of Relations 
*)

Parameter data_object_child_of_collection : el data_object -> el collection -> relation.
Parameter collection_child_of_collection : el collection -> el collection -> relation.
Parameter collection_root : el collection -> relation.
Parameter replica_of : el replica -> el data_object -> relation.
Parameter stored_at : el replica -> el resource -> relation.
Parameter resource_child_of_resource : el resource -> el resource -> relation.
Parameter resource_root : el resource -> relation.
Parameter resource_local_to_zone : el resource -> el zone -> relation.
Parameter replica_local_to_zone : el user -> el zone -> relation.
Parameter user_has_access_to_data_object : el user -> el access -> el data_object -> relation.
Parameter user_has_access_to_collection : el user -> el access -> el collection -> relation.
Parameter data_object_has_owner: el data_object -> el user -> relation.
Parameter path_of_data_object : el path -> el data_object -> relation.
Parameter id_of_data_object : el id -> el data_object -> relation.
Parameter owner_of_data_object : el user -> el data_object -> relation.
Parameter content_of_replica : el replica_content -> el replica -> relation.
Parameter path_of_collection : el path -> el collection -> relation.
Parameter id_of_collection : el id -> el collection -> relation.

(** 
** Constraints 
The types and relations must conform to certain constraints.
*)

(** 
*** List of Constraints 
*)

Axiom data_object_is_child_of_a_collection:
forall (s : observed_state) (a : el data_object), 
exists (b : el collection), is_observed (data_object_child_of_collection a b) s.

Axiom data_object_has_at_least_one_replica:
forall (s : observed_state) (a : el data_object), 
exists (b : el replica), is_observed (replica_of b a) s.

Axiom collection_is_child_of_a_collection_or_root:
forall (s : observed_state) (a : el collection), 
exists (b : el collection), is_observed (collection_child_of_collection a b) s \/ is_observed (collection_root a) s.

(**
* iRODS Interaction Model

iRODS interaction model defines the dynamic aspect of iRODS.

There are three type of interactions: actions, queries, and auxiliary functions.
*)

Parameter action : Set -> Type.
Parameter query : Set -> Set.
Parameter aux : Set -> Set.

(** 
** Actions

Actions can be applied. An applied action has the following type 
*)

Definition applied_action a := system_state -> (a + el error) * system_state.

Parameter apply_action : forall {a : Set}, action a -> applied_action a.

(**
An applied action produces a result [a], can modify the system_state, and can throw an error. 

*** List of Actions 
*)

Parameter set : relation -> action unit.
Parameter reset : relation -> action unit.
Parameter new_id : action (el id).
Parameter new_data_object : action (el data_object).
Parameter new_replica : el resource -> el path -> action (el replica).
Parameter data_object_copy : el data_object -> el path -> el connection -> action unit.
Parameter data_object_chksum : el data_object -> el connection -> action (el chksum).
Parameter data_object_rename : el data_object -> el path -> el connection -> action unit.
Parameter data_object_phymv : el data_object -> el physical_path -> el connection -> action unit.
Parameter data_object_lock : el data_object -> el connection -> action unit.
Parameter data_object_unlock : el data_object -> el connection -> action unit.
Parameter data_object_create : el resource -> el path -> el connection -> action unit.
Parameter data_object_delete : el data_object -> el connection -> action unit.
Parameter data_object_unlink : el data_object -> el path -> el connection -> action unit.
Parameter data_object_open : el data_object -> el connection -> action (el data_object_descriptor).
Parameter data_object_lseek : el data_object_descriptor -> el offset -> el connection -> action unit.
Parameter data_object_close : el data_object_descriptor -> el connection -> action unit.
Parameter data_object_read : el data_object_descriptor -> el offset -> el length -> el connection -> action (el buffer).
Parameter data_object_write : el data_object_descriptor -> el offset -> el buffer -> el connection -> action unit.
Parameter data_object_replicate : el user -> el data_object -> el resource -> el connection -> action unit.
Parameter data_object_trim : el data_object -> el resource -> el connection -> action unit.
Parameter data_object_truncate : el data_object -> el length -> el connection -> action unit.
Parameter data_object_rsync : el data_object -> el data_object -> el connection -> action unit.
Parameter data_object_get : action unit.
Parameter data_object_put : action unit.

(** 
** Queries

Queries can be lifted to an action. 
*)

Parameter lift_query : forall {a : Set}, query a -> action a.

(** 
An applied query produces a result [a] which may depend on the system state, does not modify the system_state, and can throw an error. 
*)

Axiom query_does_not_change_system_state:
forall (s : system_state) (a : Set) (qu : query a), snd (apply_action (lift_query qu) s) = s.

(** 
*** List of Queries 
*)

Parameter path_to_data_object : el path -> query (el data_object).
Parameter path_to_collection : el path -> query (el collection).
Parameter connection_user : el connection -> query (el user).

(** 
** Auxiliary Functions 

Auxiliary functions are partial functions with well-defined errors on undefined values. Therefore, it is considered part of the interaction model.

Auxiliary functions can be lifted to an action. 
*)

Parameter lift_aux : forall {a : Set}, aux a -> action a.

(** 
An applied auxiliary function produces a result [a] which does not depend on the system state, does not modify the system_state, and can throw an error. 
*)

Axiom aux_does_not_change_system_state:
forall (s : system_state) (a : Set) (au : aux a), snd (apply_action (lift_aux au) s) = s.

Axiom aux_does_not_depend_on_system_state:
forall (s1 s2 : system_state) (a : Set) (qu : query a), fst (apply_action (lift_query qu) s1) = fst (apply_action (lift_query qu) s2).


(** 
*** List of Auxiliary Functions 
*)

Parameter parent_path : el path -> aux (el path).

(** 
** DAGs of Actions

Complex actions can be composed as DAG of actions. The following combinators from "MonadPlus" give us all the ingredients we need to construct DAGs.
*)

Parameter pure : forall {a : Set}, a -> action a.
Parameter bind : forall {a b : Set}, (a -> action b) -> action a -> action b.
Parameter zero : forall {a : Set}, action a.
Parameter plus : forall {a : Set}, action a -> action a -> action a.

(**
** Constraints
The interaction model needs to satisfy the follow constraints.
*)


End iRODS_model.

(** 
* Implementation 
*)

Module Type data_types.
Parameter integer : Set.
Parameter string : Set.
Parameter tree : Set.
Parameter byte_array : Set.
Parameter empty_byte_array : byte_array.
End data_types.

Module iRODS_model_impl (dt : data_types) <: iRODS_model.

Import dt.

Definition system_state := unit.
Definition observed_state := unit.
Definition observe (s : system_state) : observed_state := s.
(** 
*** Type Names
*)

Inductive _objects := 
  | _id
  | _resc_name
  | _data_object_name
  | _file_name
  | _zone_name
  | _user_name
  | _group_name
  | _rule_name
  | _microservice_name
  | _API_name
  | _path
  | _physical_path
  | _offset
  | _length
  | _buffer
  | _chksum
  | _error
  | _replica_content
  | _access
  | _iCAT
  | _host
  | _port
  | _config
  | _AVU
  | _data_object
  | _collection
  | _resource
  | _replica
  | _user
  | _group
  | _zone
  | _metadata
  | _rule
  | _microservice
  | _PEP
  | _API
  | _connection
  | _data_object_descriptor.
  
  
Definition objects := _objects.
Definition id := _id.
Definition resc_name := _resc_name.
Definition data_object_name := _data_object_name.
Definition file_name := _file_name.
Definition zone_name := _zone_name.
Definition user_name := _user_name.
Definition group_name := _group_name.
Definition rule_name := _rule_name.
Definition microservice_name := _microservice_name.
Definition API_name := _API_name.
Definition path := _path.
Definition physical_path := _physical_path.
Definition offset := _offset.
Definition length := _length.
Definition buffer := _buffer.
Definition chksum := _chksum.
Definition error := _error.
Definition replica_content := _replica_content.
Definition access := _access.
Definition iCAT := _iCAT.
Definition host := _host.
Definition port := _port.
Definition config := _config.
Definition AVU := _AVU.
Definition data_object := _data_object.
Definition collection := _collection.
Definition resource := _resource.
Definition replica := _replica.
Definition user := _user.
Definition group := _group.
Definition zone := _zone.
Definition metadata := _metadata.
Definition rule := _rule.
Definition microservice := _microservice.
Definition PEP := _PEP.
Definition API := _API.
Definition connection := _collection.
Definition data_object_descriptor := _data_object_descriptor.

Inductive _access_type := 
  | _null 
  | _read 
  | _write 
  | _own.
  
Definition access_type := _access_type.
Definition null := _null.
Definition read := _read.
Definition write := _write.
Definition own := _own.

Definition identifier_type (obj : objects) : Set :=
  match obj with
  | _id => integer
  | _resc_name => string
  | _data_object_name => string
  | _file_name => string
  | _zone_name => string
  | _user_name => string
  | _group_name => string
  | _rule_name => string
  | _microservice_name => string
  | _API_name => string
  | _path => string
  | _physical_path => string
  | _offset => integer
  | _length => integer
  | _buffer => byte_array
  | _chksum => string
  | _error => integer * string
  | _replica_content => byte_array
  | _access => access_type
  | _iCAT => string
  | _host => string
  | _port => integer
  | _config => tree
  | _AVU => string * string * string
(** A data object is identified by a zone and an integer. *)

  | _data_object => string * integer
(** A collection is identified by a zone and an integer. *)

  | _collection => string * integer
(** A resource is identified by a zone and an integer. *)

  | _resource => string * integer

(** A replica is identified by a zone, a resource, and a path on that resource. *)

  | _replica => string * integer * string

(** A user or group is identified by a zone and a user name in that zone. *)

  | _user => string * string
  | _group => string * string
  | _zone => string

(** Metadata is identified by AVU. *)

  | _metadata => string * string * string
  
(** A rule is identified by a host and a rule name *)

  | _rule => string * string
(** A microservice is identified by a host and a microservice name *)

  | _microservice => string * string

(** A PEP is identified by a host and a PEP name *)

  | _PEP => string * string
(** An API is identified by a host and an API id *)

  | _API => string * integer
(** A connection is identified by a host and a connection id *)

  | _connection => string * integer
(** A data_object_descriptor is identified by a host and a data_object_descriptor id *)

  | _data_object_descriptor => string * integer
  end.


Definition empty_content := empty_byte_array.

(** We identify the identifier of an object with the object. *)

Definition el := identifier_type.

Definition identifier {obj : objects} (x : el obj) : identifier_type obj := x.

Lemma identifier_unique : 
forall (obj : objects) (a b : el obj),
identifier a = identifier b -> a = b.
Proof.
unfold identifier. auto.
Qed.

Inductive _relation : Set :=
  | _data_object_child_of_collection : el data_object -> el collection -> _relation
  | _collection_child_of_collection : el collection -> el collection -> _relation
  | _collection_root : el collection -> _relation
  | _replica_of : el replica -> el data_object -> _relation
  | _stored_at : el replica -> el resource -> _relation
  | _resource_child_of_resource : el resource -> el resource -> _relation
  | _resource_root : el resource -> _relation
  | _resource_local_to_zone : el resource -> el zone -> _relation
  | _replica_local_to_zone : el user -> el zone -> _relation
  | _user_has_access_to_data_object : el user -> el access -> el data_object -> _relation
  | _user_has_access_to_collection : el user -> el access -> el collection -> _relation
  | _data_object_has_owner: el data_object -> el user -> _relation
  | _path_of_data_object : el path -> el data_object -> _relation
  | _id_of_data_object : el id -> el data_object -> _relation
  | _owner_of_data_object : el user -> el data_object -> _relation
  | _content_of_replica : el replica_content -> el replica -> _relation
  | _path_of_collection : el path -> el collection -> _relation
  | _id_of_collection : el id -> el collection -> _relation.
  
Definition relation := _relation.
Definition data_object_child_of_collection := _data_object_child_of_collection.
Definition collection_child_of_collection := _collection_child_of_collection.
Definition collection_root := _collection_root.
Definition replica_of := _replica_of.
Definition stored_at := _stored_at.
Definition resource_child_of_resource := _resource_child_of_resource.
Definition resource_root := _resource_root.
Definition resource_local_to_zone := _resource_local_to_zone.
Definition replica_local_to_zone := _replica_local_to_zone.
Definition user_has_access_to_data_object := _user_has_access_to_data_object.
Definition user_has_access_to_collection := _user_has_access_to_collection.
Definition data_object_has_owner := _data_object_has_owner.
Definition path_of_data_object := _path_of_data_object.
Definition id_of_data_object := _id_of_data_object.
Definition owner_of_data_object := _owner_of_data_object.
Definition content_of_replica := _content_of_replica.
Definition path_of_collection := _path_of_collection.
Definition id_of_collection := _id_of_collection.
  
Parameter is_system : relation -> system_state -> Prop.
Parameter is_observed : relation -> observed_state -> Prop.

Hypothesis data_object_is_child_of_a_collection:
forall (s : observed_state) (a : el data_object), 
exists (b : el collection), is_observed (data_object_child_of_collection a b) s.

Hypothesis data_object_has_at_least_one_replica:
forall (s : observed_state) (a : el data_object), 
exists (b : el replica), is_observed (replica_of b a) s.

Hypothesis collection_is_child_of_a_collection_or_root:
forall (s : observed_state) (a : el collection), 
exists (b : el collection), is_observed (collection_child_of_collection a b) s \/ is_observed (collection_root a) s.

(** *** Queries *)

Inductive _query : Set -> Set :=
  | _path_to_data_object : el path -> _query (el data_object)
  | _path_to_collection : el path -> _query (el collection)
  | _connection_user : el connection -> _query (el user).
  
Definition query := _query.
Definition path_to_data_object := _path_to_data_object.
Definition path_to_collection := _path_to_collection.
Definition connection_user := _connection_user.

(** *** Auxiliary Functions *)

Inductive _aux : Set -> Set :=
  | _parent_path : el path -> _aux (el path).
  
Definition aux := _aux.
Definition parent_path := _parent_path.

(** *** Actions *)

Inductive _action : Set -> Type :=
  | _pure : forall {a : Set}, a -> _action a
  | _bind : forall {a b : Set}, (a -> _action b) -> _action a -> _action b
  | _zero : forall {a : Set}, _action a
  | _plus : forall {a : Set}, _action a -> _action a -> _action a
  | _set : relation -> _action unit
  | _reset : relation -> _action unit
  | _new_id : _action (el id)
  | _new_data_object : _action (el data_object)
  | _new_replica : el resource -> el path -> _action (el replica)
  | _lift_query : forall {a : Set}, query a -> _action a
  | _lift_aux : forall {a : Set}, aux a -> _action a.
  
Definition action := _action.
Definition pure : forall {a : Set}, a -> _action a := @_pure.
Definition bind : forall {a b : Set}, (a -> _action b) -> _action a -> _action b := @_bind.
Definition plus : forall {a : Set}, _action a -> _action a -> _action a := @_plus.
Definition zero : forall {a : Set}, _action a := @_zero.
Definition set := _set.
Definition reset := _reset.
Definition new_id := _new_id.
Definition new_data_object := _new_data_object.
Definition new_replica := _new_replica.
Definition lift_query : forall {a : Set}, query a -> _action a := @_lift_query.
Definition lift_aux : forall {a : Set}, aux a -> _action a := @_lift_aux.

Definition applied_action a := system_state -> (a + el error) * system_state.

Parameter apply_action : forall {a : Set}, action a -> applied_action a.

Hypothesis query_does_not_change_system_state:
forall (s : system_state) (a : Set) (qu : query a), snd (apply_action (lift_query qu) s) = s.

Hypothesis aux_does_not_change_system_state:
forall (s : system_state) (a : Set) (au : aux a), snd (apply_action (lift_aux au) s) = s.

Hypothesis aux_does_not_depend_on_system_state:
forall (s1 s2 : system_state) (a : Set) (qu : query a), fst (apply_action (lift_query qu) s1) = fst (apply_action (lift_query qu) s2).

(** MonadPlus notations *)

Notation "a >>= b" := (bind b a) (at level 10).
Notation "a | b" := (plus a b) (at level 11).



(** *** List of Actions *)
(** Some auxiliary definitions *)

Definition set1 {a: Set} (r1 : a -> relation) (acta : action a) : action unit :=
  acta >>= fun a0 => set (r1 a0).
Definition set2 {a b: Set} (r2 : a -> b -> relation) (acta : action a) (actb : action b) : action unit :=
  acta >>= fun a0 => actb >>= fun b0 => set (r2 a0 b0).
Definition set3 {a b c: Set} (r3 : a -> b -> c -> relation) (acta : action a) (actb : action b) (actc : action c) : action unit :=
  acta >>= fun a0 => actb >>= fun b0 => actc >>= fun c0 => set (r3 a0 b0 c0).

Definition lift_query1 {a b: Set} (q1 : a -> query b) (acta : action a) : action b :=
  acta >>= fun a0 => lift_query (q1 a0).

(** *** Data Object Create *)

Definition data_object_create (r : el resource) (p : el path) (c : el connection) :=
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
*)

Parameter data_object_copy : el data_object -> el path -> el connection -> action unit.
Parameter data_object_chksum : el data_object -> el connection -> action (el chksum).
Parameter data_object_rename : el data_object -> el path -> el connection -> action unit.
Parameter data_object_phymv : el data_object -> el physical_path -> el connection -> action unit.
Parameter data_object_lock : el data_object -> el connection -> action unit.
Parameter data_object_unlock : el data_object -> el connection -> action unit.
Parameter data_object_delete : el data_object -> el connection -> action unit.
Parameter data_object_unlink : el data_object -> el path -> el connection -> action unit.
Parameter data_object_open : el data_object -> el connection -> action (el data_object_descriptor).
Parameter data_object_lseek : el data_object_descriptor -> el offset -> el connection -> action unit.
Parameter data_object_close : el data_object_descriptor -> el connection -> action unit.
Parameter data_object_read : el data_object_descriptor -> el offset -> el length -> el connection -> action (el buffer).
Parameter data_object_write : el data_object_descriptor -> el offset -> el buffer -> el connection -> action unit.
Parameter data_object_replicate : el user -> el data_object -> el resource -> el connection -> action unit.
Parameter data_object_trim : el data_object -> el resource -> el connection -> action unit.
Parameter data_object_truncate : el data_object -> el length -> el connection -> action unit.
Parameter data_object_rsync : el data_object -> el data_object -> el connection -> action unit.
Parameter data_object_get : action unit.
Parameter data_object_put : action unit.

End iRODS_model_impl.


