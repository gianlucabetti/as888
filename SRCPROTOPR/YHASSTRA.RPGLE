000001160104     P*--------------------------------------------------
000002160107     P* Procedure name:
000003160104     P* Purpose:
000004160104     P* Returns:        Esito.
000005160104     P*--------------------------------------------------
000006160104     D Assegni_Traenza_Init...
000007160104     D                 PR            10I 0
000008160104     P*--------------------------------------------------
000009160107     P* Procedure name:
000010160104     P* Purpose:
000011160104     P* Returns:        Esito.
000012160104     P*--------------------------------------------------
000013160104     D Assegni_Traenza_New_File...
000014160104     D                 PR            10I 0
000015160104     D pi_iidd                       35    varying
000016160128     D pi_societa                     3
000017160128     D pi_ndt                         9p 0
000018160128     D pi_ddt                          d
000019160128     D pi_serpar                      4
000020160128     P*--------------------------------------------------
000021160107     P* Procedure name:
000022160104     P* Purpose:
000023160104     P* Returns:        Esito.
000024160104     P*--------------------------------------------------
000025160104     D Assegni_Traenza_End_File...
000026160104     D                 PR            10I 0
000027160107     P*--------------------------------------------------
000028160107     P* Procedure name:
000029160107     P* Purpose:
000030160107     P* Returns:        Esito.
000031160107     P*--------------------------------------------------
000032160107     D Assegni_Traenza_New_Row_Type_A...
000033160107     D                 PR            10I 0
000034160322     D  pi_banca                            like(yhbos00f.bonbanca)
000035160107     P*--------------------------------------------------
000036160107     P* Procedure name:
000037160107     P* Purpose:
000038160107     P* Returns:        Esito.
000039160107     P*--------------------------------------------------
000040160107     D Assegni_Traenza_New_Row_Type_B...
000041160107     D                 PR            10I 0
000042160107     D  pi_abi                              like(yhbos00f.bonabi)
000043160107     D  pi_cab                              like(yhbos00f.boncab)
000044160107     D  pi_indirizzo                        like(yhbos00f.bonvia)
000045160107     D  pi_cap                              like(yhbos00f.boncap)
000046160107     D  pi_localita                         like(yhbos00f.boncit)
000047160107     D  pi_prov                             like(yhbos00f.bonprv)
000048160107     D  pi_numeroAss                        like(yhbos00f.bonnra)
000049160107     D  pi_dataass                          like(yhbos00f.bondta)
000050160118     D  pi_importoAss                11  2
000051160107     D  pi_ragsoc                           like(yhbos00f.bonrag)
000052160908     D  pi_ragsocDest                       like(yhbos00f.bonrag)
000053160107     D  pi_sifcsc                           like(ansif00f.sifcsc)
000054160108     D  pi_testoassegno...
000055160108     D                             3600a
000056160107     P*--------------------------------------------------
000057160107     P* Procedure name:
000058160107     P* Purpose:
000059160107     P* Returns:        Esito.
000060160107     P*--------------------------------------------------
000061160107     D Assegni_Traenza_New_Row_Type_Z...
000062160107     D                 PR            10I 0
000063160107     P*--------------------------------------------------
000064160107     P* Procedure name:
000065160107     P* Purpose:
000066160107     P* Returns:        Esito.
000067160107     P*--------------------------------------------------
000068160107     D Assegni_Traenza_End_Row...
000069160107     D                 PR            10I 0
000070160107
