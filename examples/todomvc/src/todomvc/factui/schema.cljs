(ns todomvc.factui.schema)


(def schema
  [{:db/ident :global/tasks
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/many
    :db/doc "Attribute of the global entity listing tasks currently in the list"}
   {:db/ident :global/new-task
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one
    :db/doc "The temporary task currently being edited by the primary task input"}
   {:db/ident :global/view-mode
    :db/valueType :db.type/keyword
    :db/cardinality :db.cardinality/one
    :db/doc "The view mode for the task list. Valid values #{:all :active :completed}"}


   {:db/ident :task/name
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "The name of the task"}
   {:db/ident :task/completed
    :db/valueType :db.type/boolean
    :db/cardinality :db.cardinality/one
    :db/doc "Whether the task is marked as complete"}])