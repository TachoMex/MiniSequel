module MiniSequel.Migration
where
  import MiniSequel
  import MiniSequel.Model
  import MiniSequel.Expression

  data SequelMigration a =
    CreateTable (Model a) |
    AlterTable [SequelTableModifier] |
    CreateView SequelExpression SequelQuery

  data SequelTableModifier =
    AddColumn |
    AddIndex  |
    AlterColumn |
    RenameTable  |
    AddConstraint |
    AddKey |
    DropColumn

  data SequelSchemaVersion = SequelSchemaVersion {
    version :: Int,
    updateDate :: String
  }
