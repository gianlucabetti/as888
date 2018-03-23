000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400040719     FTIVAWWWT  UF A E             DISK
000500080924     FTIVGDTMP  UF   E             DISK
000600030924
000700080924     D fnvac00t      E DS
000800030924     D prmppt          S             50
000900030924     D prmesito        S              1
001000030924     D wrkesito        S                   like(prmesito)
001100060118
001200060118     D*-----------
001300060118     D* RIDEFINIZIONE CAMPO VACRMO SECONDO ESIGENZE CLIENTE
001400060118     D*-----------
001500060118     D E_DS_RMO        DS
001600060118     D  E_RMO_LINEA            1      4
001700060118     D  E_RMO_ORDINE           5     13
001800060118     D  E_RMO_DATA            14     21
001900030924
002000030924     D*-----------
002100040719     D* DEFINISCO LA DS DELLA STRUTTURA DELL'ARCHIVIO RITORNO ESITI DEL CLIENTE
002200030924     D*-----------
002300040719     D E_DS_Cli        DS
002400060118     D  E_GRIPMOV                     2    INZ('ES')
002500060118     D  E_GCODSOC                     2    INZ('BA')
002600060118     D  E_GNUMBOL                     8    INZ
002700060118     D  E_GANNDOC                     4    INZ
002800060118     D  E_GCODCAU                     2    INZ
002900060118     D  E_GDATEVE                     8    INZ
003000060118     D  E_GDATBOL                     8    INZ
003100060118     D  E_GCODCLI                    10    INZ
003200060118     D  E_GCLINEA                     4    INZ
003300060118     D  E_GCORDNUM                    9    INZ
003400060118     D  E_GPOPART                     3    INZ
003500060118     D  E_GPOSER                      2    INZ
003600060118     D  E_GPOSPED                     7    INZ
003700040719
003800030924
003900030325
004000030924     C* MAIN/
004100991027     C                   EXSR      traduci
004200921023     C                   SETON                                        LR
004300991027
004400030924
004500030924
004600991027     C     traduci       BEGSR
004700991027     C*
004800080924     C                   READ      TIVGDTMP
004900060119     C                   MOVEL     '0'           wScrivi           1
005000060119     C*
005100080924     C                   DOW       not %eof(TIVGDTMP)
005200080924     C*
005300080924     C                   EVAL      fnvac00t = vgdDTA
005400040719     C*
005500040719     C* Restituisco al cliente solo le bolle con consegna anomala = *blanks (sia consegnate che nn)
005600040719     C* e i resi
005700060118     C                   IF        VACCCA = *blanks OR
005800060119     C                             VACCCA = '1'
005900070820     C*
006000070820     C* Restituisco al cliente solo le bolle CON SERIE => altrimenti blocca la loro procedura d
006100070820     C* acquisizione
006200070820     C                   IF        VACNRS > *zeros
006300060118     C*
006400030925     C* Inizializzo la DS d output con i valori di default della dichiarazione della struttura dati
006500040719     C                   RESET                   E_DS_Cli
006600060118     C*
006700060118     C* Valorizzazione buffer out cliente
006800060118     C                   MOVE(P)   VACRMN        E_GNUMBOL
006900060118     C                   MOVE(P)   VACAAS        E_GANNDOC
007000060118     C                   EVAL      E_DS_RMO = VACRMO
007100060118     C                   MOVE(P)   E_RMO_DATA    E_GDATBOL
007200060118     C                   EVALR     E_GCODCLI = %trim(VACRMA)
007300060118     C                   EVAL      E_GCLINEA =  E_RMO_LINEA
007400060118     C                   MOVE(P)   E_RMO_ORDINE  E_GCORDNUM
007500060118     C                   MOVE(P)   VACLNP        E_GPOPART
007600060118     C                   MOVE(P)   VACNRS        E_GPOSER
007700060118     C                   MOVE(P)   VACNSP        E_GPOSPED
007800060118     C*
007900060119     C* Gestisco gli eventi d consegna ok
008000060118     C                   IF        VACDCM > *zeros
008100060118     C                   EVAL      E_GCODCAU = '05'
008200060118     C                   MOVE(P)   VACDCM        E_GDATEVE
008300060119     C                   EVAL      wScrivi = '1'
008400060118     C                   ENDIF
008500040719     C*
008600060118     C* Gestisco gli eventi d lasciato avviso
008700040719     C                   IF        VACDLA > VACDAG  AND
008800040719     C                             VACDLA > VACDCM
008900060118     C                   EVAL      E_GCODCAU = '06'
009000060118     C                   MOVE(P)   VACDLA        E_GDATEVE
009100080408     C                   EVAL      wScrivi = '0'
009200040719     C                   ENDIF
009300060118     C*
009400060118     C* Gestisco gli eventi d giacenza
009500040719     C                   IF        VACDAG > VACDLA  AND
009600040719     C                             VACDAG > VACDCM
009700080408     C                   EVAL      E_GCODCAU = '06'                             * ex '08'
009800060118     C                   MOVE(P)   VACDAG        E_GDATEVE
009900060119     C                   EVAL      wScrivi = '1'
010000040719     C                   ENDIF
010100930409     C*
010200030924     C* Scarico il buffer della DS cliente nel campo dati del flat-file ASCII
010300040719     C                   EVAL      VAWDTA = E_DS_Cli
010400030924     C*
010500060119     C* Se occorre ritornare al cliente l'esito corrente
010600060119     C* scarico il buffer x restituire record corrente al cliente
010700060119     C                   IF        wScrivi = '1'
010800030924     C                   WRITE     TIVAW000
010900060119     C                   ENDIF
011000070820     C                   ENDIF
011100040719     C                   ENDIF
011200040719     C*
011300040719     C* In ogni caso storicizzo e cancello l'input sempre
011400080924     C                   DELETE    TIVGD000
011500040719     C*
011600040719     C* Proseguo con la lettura dell'input
011700080924     C                   READ      TIVGDTMP
011800060119     C                   EVAL      wScrivi = '0'
011900060119     C*
012000030325     C                   ENDDO
012100991027     C*
012200030325     C                   EVAL      wrkesito = '0'
012300991027     C*
012400910830     C                   ENDSR
012500991027
012600991027      /TITLE Operazioni iniziali.
012700991027     C     *inzsr        BEGSR
012800991027     C*
012900991027     C     *ENTRY        PLIST
013000991027     C                   parm                    prmppt
013100991027     C     wrkesito      parm      wrkesito      prmesito
013200991027     C*
013300991027     C                   ENDSR
