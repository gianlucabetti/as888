000100110908     D*--------------------------------------------------
000200110908     D* Procedure name: GetAliquotaIva
000300110908     D* Purpose:        Restituisce l'aliquota IVA corretta (p.e. 20 anzich...
000400110908     D*                          è 21).
000500110908     D* Returns:        Aliquota IVA.
000600110908     D* Parameter:      priAliquotaIva => Aliquota IVA
000700110908     D* Parameter:      priDataFattura => Data emissione fattura.
000800110908     D*--------------------------------------------------
000900110908     D GetAliquotaIva  PR             5P 2
001000110908     D priAliquotaIva                 5P 2 VALUE
001100110908     D priDataFattura                  D   VALUE
