# cie_short filtra por categoria

    Code
      invalida <- cie_short("inexistente")
    Condition
      Warning:
      Categoría "inexistente" no encontrada.
      i Categorías válidas: "cardiovascular", "respiratoria", "metabolica", "gastrointestinal", "infecciosa", "oncologica", "reumatologica", "neurologica", "psiquiatrica", "traumatologica", "pediatrica" y "gineco_obstetrica"

# cie_search valida inputs

    Code
      cie_search(123)
    Condition
      Error in `cie_search()`:
      ! `text` debe ser un string character no-NA de longitud 1, no a number.

---

    Code
      cie_search("a")
    Condition
      Error in `cie_search()`:
      ! Texto mínimo 2 caracteres.

---

    Code
      cie_search("diabetes", threshold = 1.1)
    Condition
      Error in `cie_search()`:
      ! `threshold` debe estar entre 0 y 1.

---

    Code
      cie_search("diabetes", max_results = 0)
    Condition
      Error in `cie_search()`:
      ! `max_results` debe ser >= 1.

