000100980904     H*------------------------------------------------------------------------*
000200980904     H DECEDIT('0,') DATEDIT(*DMY.)
000300980904     F*------------------------------------------------------------------------*
000400980904     F* DATA BASE
000500980904     F*------------------------------------------------------------------------*
000600020606     FTISAV00F  IF   E           K DISK
000700020606
000800020606     D contaRec        S              6  0 INZ(*zeros)
000900020606
001000020606     D*---
001100020606     D* DS di procedura
001200020606     D*---
001300020606     D  TIS799DSI    E DS
001400020422
001500980904     C*------------------------------------------------------------------------*
001600980904     C* MAIN LINE
001700980904     C*------------------------------------------------------------------------*
001800980904     C*
001900020606     C* PRIMA LETTURA BOLLE
002000020606     C                   EXSR      LETFIL
002100980904     C*
002200980904     C* CICLO FINO A FINE FILE
002300980904DO  1C     $FINE         DOWEQ     'N'
002400980904     C*
002500020410     C                   EXSR      EXETAS
002600980904     C*
002700980904     C* LETTURA SUCCESSIVA BOLLE
002800980904     C                   EXSR      LETFIL
002900980904E   1C                   ENDDO                                                  *FINE FILE
003000020410     C*
003100980904     C                   SETON                                        LR
003200980904     C*------------------------------------------------------------------------*
003300980904     C* LETFIL - LETTURA PROSSIMO RECORD                                 -BOLLE-
003400980904     C*------------------------------------------------------------------------*
003500980904     C     LETFIL        BEGSR
003600980904     C*
003700980904     C* LETTURA FINO A:
003800980904     C*  - FINE FILE
003900980904     C*  - TROVATO RECORD VALIDO
004000980904     C                   MOVEL     'N'           WRECOK
004100980904DO  1C     $FINE         DOWEQ     'N'
004200980904     C     WRECOK        ANDEQ     'N'
004300020606     C                   READ      TISAV00F                               99
004400020606     C                   EVAL      contaRec = contaRec + 1
004500980904IF  2C     *IN99         IFEQ      *ON                                          *FINE ARCHIVIO
004600980904     C                   MOVEL     'S'           $FINE
004700980904X   2C                   ELSE
004800980904     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
004900980904E   2C                   ENDIF
005000980904E   1C                   ENDDO
005100980904     C*
005200980904     C                   ENDSR
005300980904     C*------------------------------------------------------------------------*
005400980904     C* CHKREC - CONTROLLA VALIDITA' RECORD                              -BOLLE-
005500980904     C*------------------------------------------------------------------------*
005600980904     C     CHKREC        BEGSR
005700020410     C*
005800020410     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
005900020606     C*
006000020606     C* Elaboro solo i primi 4421 record
006100020606     C                   IF        contaRec > 4421
006200020606     C                   MOVEL     'N'           WRECOK                         *RECORD NN VALIDO
006300020606     C                   MOVEL     'S'           $FINE                          *FINE PGM
006400020606     C                   ENDIF
006500980904     C*
006600980904     C                   ENDSR
006700980904     C*------------------------------------------------------------------------*
006800020606     C* EXETAS - AGGIORNA FILE ESTRATTO BOLLE (TIVTA00F)
006900980904     C*------------------------------------------------------------------------*
007000020410     C     EXETAS        BEGSR
007100020606     C*
007200020606     C* Impostazione ds x passaggio parametri al PGM di rigenerazione TIVTA ("Estratto Bolle")
007300020606     C                   clear                   tis799dsi
007400020606     C*
007500020606     C                   z-add     TASaas        aasI99
007600020606     C                   z-add     TASmgs        mgsI99
007700020606     C                   z-add     TASlnp        lnpI99
007800020606     C                   z-add     TASnrs        nrsI99
007900020606     C                   z-add     TASnsp        nspI99
008000020606     C                   movel     TAStbl        tblI99
008100020606     C*
008200020606     C* Chiama il programma di ri-generazione "Estratto Bolle"
008300020606     C                   call      'TIS799R'                                    *Estratto Bolle
008400020606     C                   parm                    tis799dsi
008500020410     C*
008600980904     C                   ENDSR
008700980904     C*------------------------------------------------------------------------*
008800980904     C* *INZSR - OPERAZIONI INIZIALI
008900980904     C*------------------------------------------------------------------------*
009000980904     C     *INZSR        BEGSR
009100020410     C*
009200020410     C* VARIABILI DI WRK
009300020410     C                   MOVEL     'N'           $FINE             1            *FINE PGM
009400020410     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
009500980904     C*
009600980904     C                   ENDSR
