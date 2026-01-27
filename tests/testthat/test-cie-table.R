# Tests para cie_table (tablas gt)

test_that("cie_table requiere gt instalado",
{
  skip_if_not_installed("gt")

  # Test basico con codigo valido
  tabla <- cie_table("E11")
  expect_s3_class(tabla, "gt_tbl")
})

test_that("cie_table funciona con multiples codigos", {
  skip_if_not_installed("gt")
  skip_on_cran()

  tabla <- cie_table(c("E11", "I10"))
  expect_s3_class(tabla, "gt_tbl")
})

test_that("cie_table maneja codigo invalido", {
  skip_if_not_installed("gt")
  skip_on_cran()

  # Codigo que no existe lanza error
  expect_error(cie_table("XXXXX"), "no encontrado")
})

# ==============================================================================
# PRUEBAS ADICIONALES cie_table()
# ==============================================================================

test_that("cie_table con interactive=FALSE funciona", {
  skip_if_not_installed("gt")
  skip_on_cran()

  # El parametro interactive existe aunque no se use internamente
  tabla <- cie_table("E11", interactive = FALSE)
  expect_s3_class(tabla, "gt_tbl")
})

test_that("cie_table con interactive=TRUE funciona", {
  skip_if_not_installed("gt")
  skip_on_cran()

  tabla <- cie_table("E11", interactive = TRUE)
  expect_s3_class(tabla, "gt_tbl")
})

test_that("cie_table genera header correcto", {
  skip_if_not_installed("gt")
  skip_on_cran()

  tabla <- cie_table("E11")

  # Verificar que es objeto gt
  expect_s3_class(tabla, "gt_tbl")

  # gt tables tienen estructura interna que podemos verificar
  expect_true("_heading" %in% names(tabla))
})

test_that("cie_table funciona con codigo categoria", {
  skip_if_not_installed("gt")
  skip_on_cran()

  # Categoria sin punto (expande a todos los hijos)
  tabla <- cie_table("I10")
  expect_s3_class(tabla, "gt_tbl")
})

test_that("cie_table funciona con codigo especifico", {
  skip_if_not_installed("gt")
  skip_on_cran()

  # Codigo especifico con punto
  tabla <- cie_table("E11.0")
  expect_s3_class(tabla, "gt_tbl")
})

test_that("cie_table expande jerarquia automaticamente", {
  skip_if_not_installed("gt")
  skip_on_cran()

  # E11 debe expandir a E11.0, E11.1, etc.
  tabla <- cie_table("E11")

  # Verificar que tiene datos (no solo header)
  # El objeto gt tiene _data interno
  expect_true("_data" %in% names(tabla))
  expect_gt(nrow(tabla$`_data`), 1)
})

test_that("cie_table tiene columnas esperadas", {
  skip_if_not_installed("gt")
  skip_on_cran()

  tabla <- cie_table("E11.0")

  # Verificar columnas en los datos internos
  columnas_data <- names(tabla$`_data`)

  expect_true("codigo" %in% columnas_data)
  expect_true("descripcion" %in% columnas_data)
})

test_that("cie_table error sin gt instalado", {
  # Este test solo funciona si gt NO esta instalado
  skip_if(requireNamespace("gt", quietly = TRUE),
          "gt esta instalado")

  expect_error(
    cie_table("E11"),
    "gt"
  )
})
