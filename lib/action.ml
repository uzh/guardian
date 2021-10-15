type t = [
    `Create |
    `Read |
    `Update |
    `Delete
] [@@deriving ord,show]

let manage = [`Create; `Read; `Update; `Delete]
