000100980618      ***********************************************************************
000200050914      *   Chiamato da TRTC100R                       - E.D.I.   ESTERO -    *
000300960329      ***********************************************************************
000400050914 :    * Il programma reperisce la Nazione del primo dettaglio mittente
000500050914 :    * dallo stesso ciclo di lettura del Flat file per impostare la LNP
000600050914      ***********************************************************************
000700050914 :    * es.: NLLYN che può avere + LNP e sulla tabella PT
000800050914 :    *  " NLLYN ...... NL " e " NLLYN ...... BE "
000900050914 :    *  gestisce 2 nazioni Belgio e Olanda
001000050914      *  ma noi riceviamo solo NLLYN dal mittente.
001100000126      ***********************************************************************
001200960329     H DECEDIT('0,') DATEDIT(*DMY.)
001300000120      *---------------------------------------------------------------------*
001400050914     FRCVIFCSUM iF   E             DISK    rename(rcvdati:rcvsum)
001500040302     FEDTAB01L  IF   E           K DISK
001600000120      *---------------------------------------------------------------------*
001700000120      * .. per scompozione dati ricevuti a seconda del tipo record
001800040302     D EDTT00DS      E DS
001900000120     D EDTD15DS      E DS
002000000120     D  D15                  445    500    DIM(2)
002100040302      * Tabella partner
002200040302     D CPT             S             35    DIM(100)
002300040302     D W035A           s             35
002400000120      *---------------------------------------------------------------------*
002500000120      *                    .. 5 - 8  : tipo record letto
002600050914     D WDAT            DS          1950
002700960524     D  SUMTPR                 5      8
002800000120      *---------------------------------------------------------------------*
002900000120      * Ciclo principale
003000000120      *---------------------------------------------------------------------*
003100040301      *  Parametrti ricevuti:
003200040301      *     Nrrec_a   = Num.Rel.Rec. della testata da cui partire (potebbero esserci
003300040301      *                 più testate/dettagli all'interno dello stesso Flat File
003400040301      *     Quale_Det = in quale dettaglio CZ reperire la nazione del mittente.
003500040301      *                 Solitamente lo si prende dal primo ma se ci fosse qualche
003600040301      *                 problema lo deve cercare sui successivi prendendo il primo
003700040301      *                 valido.
003800050914      *     Naz-Mitt  = è il parametro da restituire al chiamante per permettere di
003900050914      *                 trovare il mittente corretto e quindi l'LNP.
004000040301      *---------------------------------------------------------------------*
004100040302     C     *LIKE         DEFINE    TA0004        WCODPT
004200040302      *---------------------------------------------------------------------*
004300040301     c     *entry        plist
004400040301     c                   parm                    Nrec              9 0
004500040301     c                   parm                    Naz_mitt          3
004600040302      *
004700040302      * Caricamento Tabella Partner esteri
004800040302     C                   Z-ADD     0             X                 3 0
004900040302     C                   MOVEL     'PT'          TABCOD
005000040302     C     TABCOD        CHAIN     EDTAB01L                           30
005100040302     C     *IN30         DOWEQ     '0'
005200040310     C     TABFLG        ifEQ      *BLANKS
005300040302     C                   ADD       1             X
005400040302     C                   MOVEL     TABKEY        CPT(X)
005500040302     C                   End
005600040302     C     TABCOD        READE     EDTAB01L                               30
005700040302     C                   END
005800040301     C*
005900040301     C* IMPOSTO A ZERO VARIABILI DI PGM
006000040301     C                   MOVEL     'N'           WFINE             1
006100040301     C                   z-add     0             conta             3 0
006200040301      *
006300040302      *  Si posiziona legge e memorizza il Nome del Partner Mittente
006400040302     c                   exsr      X_primacosa
006500980609      *
006600960329      * Lettura fino a fine file
006700040302     C     Wfine         DOWNE     'S'                                          - Fine file -
006800980609      *
006900040301     C     SUMTPR        ifeq      'TD15'
007000040302      *               *--------------------*
007100980609     C                   EXSR      DRED15
007200040302      *               *--------------------*
007300040301      * deve fare uscire forzatamente dal ciclo appena ha l'informazione richiesta
007400040301     C                   endif
007500070208      *
007600070208      *  Se ha trovata la nazione deve uscire e restituirla al chiamante
007700070208     C     Naz_mitt      ifNE      *blank
007800070208     C                   leave
007900070208     C                   endif
008000980609      *
008100040301      * Se ha finito i dettaglio e legge una successiva testata senza avere
008200040301      *  trovato nulla deve uscire forzatamente
008300040302     C     SUMTPR        ifEQ      'TT00'
008400040302     c     conta         andGT     1
008500040301     C                   leave
008600040301     C                   endif
008700960329      * Lettura successiva
008800960329     C                   EXSR      REDFIL
008900960329     C                   END                                                    - Fine file -
009000980609      *
009100040301     c     FINE          tag
009200960329     C                   SETON                                        LR
009300960524     C*----------------------------------------------------------------
009400040302     C*? x Prima cosa: Si posiziona Legge e memorizza il nome generico del Partner
009500960524     C*----------------------------------------------------------------
009600040302     C     x_Primacosa   BEGSR
009700960524     C*
009800040302      * Posizionamento iniziale sul file
009900040302     C                   EXSR      SETFIL
010000040302      * Lettura sequenziale
010100040302     C                   EXSR      REDFIL
010200040302     C*
010300040302      * Imposta il codice Partner
010400040302     C     SUMTPR        ifeq      'TT00'
010500050914     C                   MOVEL     rrIFCSUM      EDTT00DS
010600040302     C                   MOVEL     TA0004        WCODPT
010700040302     C                   else
010800040302     C* Se il primo record letto non è la testata Partner UNB
010900040302     C*  deve uscire dal pgm senza dare nessun esito
011000040302     c                   move      'S'           Wfine
011100040302     C                   endif
011200040302     C*
011300960524     C                   ENDSR
011400040302     C*----------------------------------------------------------------
011500040302     C*? SETFIL - POSIZIONAMENTO FRA più TESTATE
011600040302     C*----------------------------------------------------------------
011700040302     C     SETFIL        BEGSR
011800040302     C* Lettura file
011900050914     C     NREC          SETLL     rcvIFCSUM
012000040302     C                   ENDSR
012100040301     C*----------------------------------------------------------------
012200040301     C*? REDFIL - LETTURA FILE
012300040301     C*----------------------------------------------------------------
012400040301     C     REDFIL        BEGSR
012500040301     C* Lettura file
012600050914     C                   READ      rcvIFCSUM                              30
012700040301     C   30              MOVEL     'S'           WFINE
012800050914     C  N30              MOVEL     RRIFCSUM      WDAT
012900040301     C                   ENDSR
013000010502     C*----------------------------------------------------------------
013100960527     C*? DRED15 - DECODIFICA TIPO RECORD TD15
013200960527     C*----------------------------------------------------------------
013300960527     C     DRED15        BEGSR
013400000120      *
013500050914     C                   MOVEL     rrIFCSUM      EDTD15DS
013600980930      * Dati mittente
013700040301     C                   IF         DD3035 = 'CZ' and
013800040301     C                              DD3207 <> *blanks
013900040301     C                   add       1             conta
014000040302      * Verifica esistenza su tab.:PT
014100040302     C                   eval      W035A = %subst(WCODPT:1:31) + DD3207
014200040302     C                   z-add     1             X
014300040302     C     W035a         lookup    CPT(X)                                 32
014400040302     C                   If        *in32 = *on
014500040302     c                   movel     DD3207        Naz_mitt
014600040302     c                   endIF
014700000125     C                   ENDIF
014800031211      *
014900960527     C                   ENDSR
