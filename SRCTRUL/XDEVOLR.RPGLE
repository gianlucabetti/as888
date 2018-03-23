000100000414
000200000414     H DECEDIT('0,') DATEDIT(*DMY.)
000300000414
000400030716     D*------------
000500030716     D* PARAMETRI DI CHIAMATA
000600030716     D*------------
000700030716     D IN_Volume       S             30  9
000800030716     D IN_Step         S             30  9
000900030717     D IN_FlgInt       S              1
001000030717     D OUT_LatoL       S             30  9
001100030717     D OUT_LatoH       S             30  9
001200030717     D OUT_LatoW       S             30  9
001300030716     D OUT_Errore      S              1
001400030716
001500030716
001600030716
001700000414     C*------------------------------------------------------------------------*
001800000414     C* MAIN LINE
001900000414     C*------------------------------------------------------------------------*
002000030716     C*
002100030716     C* Effettua solamente la chiamata monitorizzata al *pgm che esegue il calcolo vero e proprio
002200030717     C                   CALL(e)   'XDEVOLR1'
002300030716     C                   PARM                    IN_Volume
002400030716     C                   PARM                    IN_Step
002500030717     C                   PARM                    IN_FlgInt
002600030717     C                   PARM                    OUT_LatoL
002700030717     C                   PARM                    OUT_LatoH
002800030717     C                   PARM                    OUT_LatoW
002900030716     C                   PARM                    OUT_Errore
003000030716     C*
003100030716     C* Gestisco l'esito
003200030716     C                   IF        %error
003300030717     C                   EVAL      OUT_LatoL   = *zeros
003400030717     C                   EVAL      OUT_LatoH   = *zeros
003500030717     C                   EVAL      OUT_LatoW   = *zeros
003600030716     C                   EVAL      OUT_Errore = '1'
003700030716     C                   ENDIF
003800000414     C*
003900000414     C                   SETON                                        LR
004000000411
004100000414     C*------------------------------------------------------------------------*
004200000414     C* *INZSR - OPERAZIONI INIZIALI
004300000414     C*------------------------------------------------------------------------*
004400000414     C     *INZSR        BEGSR
004500030716     C*
004600030716     C* Ricevimento parametri
004700000414     C     *ENTRY        PLIST
004800030716     C                   PARM                    IN_Volume
004900030716     C                   PARM                    IN_Step
005000030717     C                   PARM                    IN_FlgInt
005100030717     C                   PARM                    OUT_LatoL
005200030717     C                   PARM                    OUT_LatoH
005300030717     C                   PARM                    OUT_LatoW
005400030716     C                   PARM                    OUT_Errore
005500030716     C*
005600000414     C                   ENDSR
