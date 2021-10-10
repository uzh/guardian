type t = [
    `Create |
    `Read |
    `Update |
    `Delete
] [@@deriving ord]

let manage = [`Create; `Read; `Update; `Delete]
