000100990127      *----------------------------------------------------*
000200990217      *  Aggiornamento flag DCTTCR "NON CONSIDERARE LA RESPONSABILITA''
000300990127      *----------------------------------------------------*
000400990127
000500990127      ****************************************************************
000600990127      *  RIEPILOGO INDICATORI
000700990127      ***************************************************************
000800990127      * 30 - COMODO
000900990209      * 31 - COMODO
001000990217      * 32 - COMODO
001100990305      * 33 - COMODO
001200990217      * 40 - tipo anomalia presente in tabella anomalia da escludere
001300990209      * 90 - prima volta che stampo
001400990127      ***************************************************************
001500990127
001600990127     H DECEDIT('0,') DATEDIT(*DMY.)
001700990127
001800990217     Ftntbe01l  iF   E           K DISK
001900990217     Ftntbe02l  iF   E           K DISK
002000990217     F                                     RENAME(tntbe000:tntbe002)
002100990217     FFNDCT01L  UF   E           K DISK
002200990305     FFNDET01L  IF   E           K DISK
002300990218     FQPRINT    O    F  132        PRINTER oflind(*INOF)
002400990127
002500990127      *------------------------------------------------------------------------*
002600990202     D dateu           s              8  0
002700990202     D wdtgio          S              8  0
002800990202     D w0140           S             14  0
002900990209     D ggspe           S              2  0
003000990209     D mmspe           S              2  0
003100990305     D wtad            S              2
003200990209     D TRAT            S            132    INZ(*all'-')
003300990217     D KTA             S              2    DIM(50)                              Chiave TAD
003400990217     D Kke1            S                   LIKE(tbeke1)
003500990217     D Kke2            S                   LIKE(tbeke2)
003600990217     D Klin            S                   LIKE(tbelin)
003700990217     D Ksif            S                   LIKE(tbesif)
003800990217     D Kcod            S                   LIKE(tbecod)
003900990202      *
004000990217     D DSNCA           DS
004100990217     d  waac                   1      4  0
004200990217     d  wfil                   5      7  0
004300990217     d  wnca                   8     14  0
004400990629     D                 DS
004500990629     D  dctaac
004600990629     D  dctmgc
004700990629     D   DCTDCA                1      8  0
004800990217      *
004900990217     d  wkeyca         S             14
005000990217      *
005100990217     D dtad          E DS
005200990202      *
005300990209     D CNCR80        E DS
005400990209     D UT§DSE0F      E DS
005500990202     D KPJBA         E DS
005600990208      *
005700990208     D WLBDAT          DS                  INZ
005800990208     D  G02DAT                 1      8  0
005900990208     D  G02INV                 9     16  0
006000990208     D  G02ERR                17     17
006100990208     D  G02TGI                18     22  0
006200990209
006300990209     D TES             S            100    DIM(3) CTDATA PERRCD(1)              TESTATA STAMPA
006400990209
006500990202      *****************************************************************
006600990202      *
006700990217     c*
006800990217      * se è stato passato un numero CA  aggancio la singola CA
006900990217     c                   if        wkeyca <> *blanks
007000990217     c                   movel     wkeyca        dsnca
007100990217     c* leggo la singola CA
007200990217     c     kdct          chain     fndct01l                           32
007300990217     c*
007400990217     c  n32              exsr      sub_agg
007500990217     c*
007600990217     c                   else
007700990217     c*
007800990202     c* Lettura di tutte le CA
007900990202      *
008000990202     c                   do        *hival
008100990202      *
008200990217     c                   read      fndct01l                               32
008300990217     c*
008400990217     c  n32              exsr      sub_agg
008500990217     c*
008600990202      *
008700990217     c  N32              enddo
008800990202      *
008900990217     c                   endif
009000990209     c*
009100990202     c                   eval      *inlr = *on
009200990217      *---------------------------------------------------------------*
009300990217      *   SUB_AGG   Routine aggiornamento Testata CA
009400990217      *---------------------------------------------------------------*
009500990217     C     SUB_AGG       BEGSR
009600990217     c*
009700990217      * se CA con :
009800990628      *            - tipo anomalia presente nella schiera
009900990628      *              non considero la responsabilita'
010000990217      *
010100990305     c* se c'è evento recupero il tipo anomalia dell'evento
010200990305     c                   if        dctnev > 0
010300990305     c     kdet          chain     fndet01l                           33
010400990305     c                   if        not *in33
010500990305     c                   eval      wtad = dettad
010600990305     c*
010700990305     c*******************************************************************************************
010800990305     c*
010900990305     c**** wtad          lookup    kta                                    40
011000990305     c* se 40 acceso non deve essere calcolata la responsabilità ma se è spento controllo il tipo
011100990305     c* anomalia della CA
011200990305     c****               if        *in40 = *off
011300990305     c****               eval      wtad = dcttad
011400990305     c****               endif
011500990305     c*
011600990305     c                   else
011700990305     c                   eval      wtad = dcttad
011800990305     c                   endif
011900990305     c*
012000990305     c                   else
012100990305     c* se non c'è evento recupero il tipo anomalia dalla testata CA
012200990305     c                   eval      wtad = dcttad
012300990305     c                   endif
012400990217     c* controllo il tipo anomalia se è presente tra quelle della schiera
012500990305     c     wtad          lookup    kta                                    40
012600990305     c*
012700990217     c* per 40 acceso anomalia presente
012800990629     c                   if        dctres = 'P' and dctdca<19990601 or
012900990629     c                             *in40 = *on
013000990217      *
013100990217     c                   eval      dcttcr = 'S'
013200990217      *
013300990629      * se invece non si verifica neppure uno dei casi precedenti puli sco il campo
013400990629     c                   else
013500990217      *
013600990217     c                   eval      dcttcr = ' '
013700990217      *
013800990217     c                   endif
013900990217     c*
014000990217     c* aggiorno il file
014100990217     c                   except    upddct
014200990217     c*
014300990217     c                   if        not *in90 or *inof
014400990217     c*
014500990308     c*                  except    testa
014600990217     c                   eval      *in90 = *on
014700990217     c                   eval      *inof = *off
014800990217     c                   endif
014900990217      *
015000990308     c*                  except    riga
015100990217      *
015200990202     c                   endsr
015300990127      *****************************************************************
015400990127      *   ROUTINE INIZIALE
015500990127      *****************************************************************
015600990127     C     *INZSR        BEGSR
015700990127      *
015800990127     C     *ENTRY        PLIST
015900990127     C                   PARM                    KPJBA
016000990217     c                   parm                    wkeyca
016100990209      *
016200990209     C                   Z-ADD     1             CODUT
016300990209     C                   CALL      'X§PARUT'
016400990209     C                   PARM                    UT§DSE0F
016500990209     C                   MOVEL     RAGUT         RSUT             20
016600990209     C                   MOVEL     REC80         CNCR80
016700990208      * ORA
016800990208     C                   TIME                    W0140
016900990208      * UDATE IN GGMMAAAA
017000990208     C                   MOVE      W0140         WDTGIO
017100990305      * accesso Testata Evento
017200990305     C     kdet          KLIST
017300990305     C                   KFLD                    dctaae
017400990305     C                   KFLD                    dctnev
017500990217      * accesso Testata CA
017600990217     C     kdct          KLIST
017700990217     C                   KFLD                    waac
017800990217     C                   KFLD                    wfil
017900990217     C                   KFLD                    wnca
018000990305      * accesso Tabella
018100990217     C     KTbe1         KLIST                                                  *
018200990217     C                   KFLD                    kcod                           *
018300990217     C                   KFLD                    kke1                           *
018400990217     C                   KFLD                    kke2                           *
018500990217     C                   KFLD                    klin                           *
018600990217     C                   KFLD                    ksif                           *
018700990217     c*
018800990217     C     KTbe2         KLIST                                                  *
018900990217     C                   KFLD                    kcod                           *
019000990217     C                   KFLD                    klin                           *
019100990217     C                   KFLD                    ksif                           *
019200990217      * carico la schiera tipo anomalia con raggruppamento AVARIA da evitare
019300990208      *
019400990217     c                   z-add     *zeros        xx                2 0
019500990217     c                   move      *blanks       kcod
019600990217     c                   move      *blanks       klin
019700990217     c                   move      knsif         ksif
019800990217     c                   movel     *zeros        kke1
019900990217     c                   move      'TAD'         kke1
020000990217     c                   move      *blanks       kke2
020100990217     C     Ktbe1         chain     tntbe01l                           31
020200990217     c* se non l'ho trovato cerco senza il sistema informativo specifico
020300990217     c                   if        *in31 = *on
020400990217     c                   move      *blanks       ksif
020500990217     C     Ktbe1         chain     tntbe01l                           31
020600990217     c                   endif
020700990217     c* imposto la chiave a parte il sistema informativo che rimane uguale all'ultima
020800990217     c* chain fatta
020900990217     c                   move      'TAD'         kcod
021000990217     c                   move      *blanks       klin
021100990217     C*
021200990217     C*-------- CARICA SCHIERA ----------------------------*
021300990217     C*                        SISTEMA INFORMATIVO  VALORIZZATO come da testata tabella
021400990217     C     Ktbe2         SETLL     tntbe02l                                     *
021500990217     C     Ktbe2         READE     tntbe02l                               30
021600990217      *
021700990217     c                   DOW       *in30 = *off
021800990217     c                   movel     TBEuni        DTAD
021900990305      * Se il S.I. è indicato deve essere uguale al mio
022000990305      *
022100990305      * Carico in tabella le anomalie che hanno il flag calcolo responsabilità = ' '
022200990305     c                   IF        TBEatb = *BLANKS  and  §TADcare = ' '
022300990217      *
022400990217     c                   add       1             xx
022500990217     c                   movel     TBEke1        KTA(XX)
022600990217     c                   endif
022700990217     c     Ktbe2         reade     tntbe02l                               30
022800990217     c                   enddo
022900990217     c*
023000990127     C                   ENDSR
023100990217     c*----------------------------------------------------------------------
023200990217     Ofndct000  E            upddct
023300990217     O                       dcttcr
023400990217     OQPRINT    E            TESTA             2
023500990209     O                       RSUT              +  0
023600990209     O                                         +  0 '/'
023700990209     O                       SIMFEL            +  0
023800990209     O                                           56 'ELENCO CA / SPEDIZIONI CON'
023900990217     O                                         +  1 'CAMBIO DCTTCR RESPONSABILI'
024000990217     O                                              'TA'''
024100990628     O                                          100 'FIDN52R'
024200990209     O                       WDTGIO        Y    115
024300990209     O                                          125 'PAG.'
024400990209     O                       PAGE          Z    130
024500990209     O          E            TESTA             3
024600990209     O                       KNSIF             +  0
024700990209     O                       KNMUS             +  1
024800990209     O          E            TESTA             4
024900990209     O                       TRAT               132
025000990209     O          E            TESTA             5
025100990209     O                       tes(1)
025200990217     O          E            TESTA          1  6
025300990209     O                       TRAT               132
025400990217     O          E            riga           1
025500990209     O                       dctaac
025600990209     O                       dctfil           +   1
025700990209     O                       dctnca        Z  +   1
025800990217     O                       dcttcr           +   5
025900990305     o                       wtad             +   5
026000990217     o                       dctaae        z  +   3
026100990217     o                                              '/'
026200990217     o                       dctnev        z
026300990217     o                       dctres           +   8
026400990217     O          E            fine           1
026500990209     o                       Tes(3)           +  30
026600990217     O          E            fine           1
026700990209     o                       Tes(2)           +  30
026800990217     O          E            fine           1
026900990209     o                       Tes(3)           +  30
027000990209** TES  (Lungh. 100)   -Disegno stampa-
027100990217Anno P.O.  N°C.A.  Flag Anom.  Evento     Resp.Partner
027200990209MANDARE LA LISTA IN SEDE ALL'UFF. CED
027300990209-------------------------------------
