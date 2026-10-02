# cie10_sql bloquea DROP TABLE

    Code
      cie10_sql("DROP TABLE cie10")
    Condition
      Error in `cie10_sql()`:
      ! Solo consultas `SELECT` permitidas (seguridad).

# cie10_sql bloquea DELETE

    Code
      cie10_sql("DELETE FROM cie10 WHERE codigo = 'E11.0'")
    Condition
      Error in `cie10_sql()`:
      ! Solo consultas `SELECT` permitidas (seguridad).

# cie10_sql bloquea UPDATE

    Code
      cie10_sql("UPDATE cie10 SET descripcion = 'test' WHERE codigo = 'E11.0'")
    Condition
      Error in `cie10_sql()`:
      ! Solo consultas `SELECT` permitidas (seguridad).

# cie10_sql bloquea INSERT

    Code
      cie10_sql(
        "INSERT INTO cie10 VALUES ('X99', 'test', NULL, NULL, NULL, NULL, NULL, NULL, 0, 0)")
    Condition
      Error in `cie10_sql()`:
      ! Solo consultas `SELECT` permitidas (seguridad).

