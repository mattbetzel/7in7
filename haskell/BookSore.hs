data Book = Book Int String [String]
                deriving (Show)
data Magazine = Magazine Int String [String]
                deriving (Show)

type CustomerID = Int
type ReviewBody = String
type CardHolder = String
type CardNumber = String
type Address = [String]

data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                   deriving (Show)

myInfo = Book 9780135072455 "Algebra of Programming" ["Richard Bird", "Oege de Moor"]

show (Book a b c) = b
