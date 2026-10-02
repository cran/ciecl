# cie11_search requiere API key

    Code
      cie11_search("diabetes")
    Condition
      Error in `cie11_search()`:
      ! API key OMS requerida. Ver: <https://icd.who.int/icdapi>

# cie11_search valida formato de API key

    Code
      cie11_search("diabetes", api_key = "invalid_key_format")
    Condition
      Error in `cie11_search()`:
      ! API key debe tener formato "client_id:client_secret"

