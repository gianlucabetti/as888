000100980521      ****************************************************************
000200980521      *  RIEPILOGO INDICATORI
000300981008      ****************************************************************
000400990603      * 01 -
000500990317      ****************************************************************
000600980521     H DECEDIT('0,') DATEDIT(*DMY.)
000700980623
000800990602      *------------------------------------------------------------------------*
000900990603     ftnssl5d   cf   E             workstn
000901081002     FTABEL00F  IF   E           K DISK
001000980521      *------------------------------------------------------------------------*
001100980702
001200990602      * Definizione campi di work ------------------------------------------
001201081002     d ktbcod          s                   like(tblcod)
001202081003     d ktbkey          s                   like(tblkey)
001300990609     D XX              s              3  0
001400990603     d dini_eur        s               D   datfmt(*eur)
001500990603     d dini_iso        s               D   datfmt(*iso)
001600990603     d dfin_eur        s               D   datfmt(*eur)
001700990603     d dfin_iso        s               D   datfmt(*iso)
001800990603     D wtibs69r        s              1
001801120913     d*dataiso         s               d   datfmt(*iso)
001802120913     d*wdata           s              8  0
001900990602      * Schiere ------------------------------------------------------------
002000081003     D MSG             S             78    DIM(5) CTDATA PERRCD(1)
002100990609     d vksc            s              7    dim(10)
002200990603     d dksc            s             20    dim(10)
002300990602      * DS esterne ---------------------------------------------------------
002400990609     d DSXc          E DS                  extname(xclirds)
002500990603     d TIBS69DS      E DS
002600990609     d DS_cnaco      E DS                  extname(CNACO00F)
002700990603     d DS_cnind      E DS                  extname(CNIND00F)
002800990603     d DS_cnclp      E DS                  extname(CNCLP00F)
002900990603     d DS_fncls      E DS                  extname(FNCLS00F)
003000990602     D tnssl5ds      E DS
003100990609     d ksc                            7    dim(10) overlay(tnssl5ds:22)
003200990602
003300990602     D CNCR80        E DS
003400990603     D UT§DSE        E DS                  extname(ut§dse0f)
003500990609     d  tcu                  398    697    DIM(50)
003600990609     d  kcu                  698    847P 0 DIM(50)  PACKEVEN
003601081002     D DS$3          E DS
003602120913     D*DS5A          E DS
003700990602     D KPJBA         E DS
003800990602      * DS interne ---------------------------------------------------------
003900990609     d                 ds
004000990609     ddsksn                    1      4p 0
004100990609     ddsksa                    1      4
004200990609     d TCUDS           DS
004300990609     d  f1                     1      1
004400990609     d  f3                     3      3
004500990609     d  f2                     2      2
004600990609     d  f4                     4      4
004700990609     d  f56                    5      6
004800990602
004900980525      *
005000990601
005100990603     Issl5d01
005200990603     I              v1cks0                      vksc(1)
005300990603     I              v1cks1                      vksc(2)
005400990603     I              v1cks2                      vksc(3)
005500990603     I              v1cks3                      vksc(4)
005600990603     I              v1cks4                      vksc(5)
005700990603     I              v1cks5                      vksc(6)
005800990603     I              v1cks6                      vksc(7)
005900990603     I              v1cks7                      vksc(8)
006000990603     I              v1cks8                      vksc(9)
006100990603     I              v1cks9                      vksc(10)
006200990603     I              v1dks0                      dksc(1)
006300990603     I              v1dks1                      dksc(2)
006400990603     I              v1dks2                      dksc(3)
006500990603     I              v1dks3                      dksc(4)
006600990603     I              v1dks4                      dksc(5)
006700990603     I              v1dks5                      dksc(6)
006800990603     I              v1dks6                      dksc(7)
006900990603     I              v1dks7                      dksc(8)
007000990603     I              v1dks8                      dksc(9)
007100990603     I              v1dks9                      dksc(10)
007200980525      *
007300990601
007400990603     c                   clear                   v1cdti
007500990603     c                   clear                   v1cdtf
007600990603     c                   clear                   vksc
007700990603     c                   clear                   dksc
007800990616     c*                  clear                   v1cbar
007801150320     c                   clear                   v1cr12
007802150323     c                   clear                   v1catf
007900990603     c     emetti        tag
008000990603     c                   exfmt     ssl5d01
008100990603     c                   setoff                                       2829
008200990603     c     *inkc         cabeq     '1'           fine
008300990603     c                   exsr      ctrd01
008400990603     c   28
008500990603     cornkf
008600990603     cor 29              goto      emetti
008700990603     c                   move      v1cdti        dl5gmi
008800990603     c                   move      dini_iso      dl5dti
008900990604     c                   move      v1cdtf        dl5gmf
009000990604     c                   move      dfin_iso      dl5dtf
009100990604     c     v1cdtf        ifeq      *zeros
009200990604     c                   move      v1cdti        dl5gmf
009300990604     c                   move      dini_iso      dl5dtf
009400990604     c                   end
009500990609     c                   do        10            ii
009600990609     c     vksc(ii)      ifeq      *blanks
009700990609     c                   move      *zeros        vksc(ii)
009800990609     c                   endif
009900990609     c                   enddo
010000990603     c                   movea     vksc          ksc
010100990616     c*                  movel     v1cbar        dl5ops
010101150320     c                   movel     v1cr12        dl5r12
010103150320     c                   if        v1cr12=*blanks
010104150320     c                   movel     'N'           dl5r12
010105150320     c                   endif
010106150323     c                   movel     v1catf        dl5atf
010107150323     c                   if        v1catf=*blanks
010108150323     c                   movel     'N'           dl5atf
010109150323     c                   endif
010200990603     c                   movel     tnssl5ds      kpjbu
010300990603     c                   movel     'SSL6'        kcoaz
010400150323     c                   call      'BCH10'
010401150323     c***                call      'TNSSL6R'
010500990603     c                   parm                    kpjba
010600990603     c     fine          tag
010700990603     c*
010800990603     C                   IF        Wtibs69r  <>  *BLANKS
010900990603     C                   EVAL      I69TLA  = 'C'
011000990603     C                   CALL      'TIBS69R'
011100990603     C                   PARM                    TIBS69DS
011200990603     C                   ENDIF
011300990603
011400990603     c                   seton                                        lr
011500990602
011600990603     c**********************************************************************
011700990603     c     *inzsr        begsr
011800990603
011900990603     C     *entry        plist
012000990603     C                   parm                    kpjba
012100990603
012200990603     C                   Z-ADD     1             CODUT
012300990603     C                   CALL      'X§PARUT'
012400990603     C                   PARM                    UT§DSe
012500990603     C                   MOVEL     RAGUT         RSUT
012600990603     C                   MOVEL     REC80         CNCR80
012700990609      *--- RICERCA CAPOCONTI
012800990609     C                   DO        50            XX
012900990609     C                   MOVE      TCU(XX)       TCUDS
013000990609     C     f56           CABNE     'CG'          END1
013100990609     C     f4            COMP      '1'                                    21
013200990609     C     f4            COMP      '2'                                    22
013300990609     C     f4            COMP      '3'                                    23
013400990609     C     f4            COMP      '6'                                    27
013500990609     C** 1 CLIENTI   21
013600990609     C** 2 FORNITORI 22
013700990609     C** 3 AGENTI    23
013800990609     C     f3            comp      '0'                                242425
013900990609     C     f3            comp      'I'                                    26
014000990609     C** 0 ITALIA   25
014100990609     C** 1 ESTERO   24
014200990609     ** I CAPO CONTO IVA
014300990609     c                   if        *in21 = *on
014400990609     C   24              Z-ADD     KCU(XX)       KCE               4 0
014500990609     C   25              Z-ADD     KCU(XX)       KCI               4 0
014600990609     c                   end
014700990609     c                   if        *in22 = *on
014800990609     C   24              Z-ADD     KCU(XX)       KFE               4 0
014900990609     C   25              Z-ADD     KCU(XX)       KFI               4 0
015000990609     c                   end
015100990609     c                   if        *in23 = *on
015200990609     C   24              Z-ADD     KCU(XX)       KAE               4 0
015300990609     C   25              Z-ADD     KCU(XX)       KAI               4 0
015400990609     c                   end
015500990609     C   26              Z-ADD     KCU(XX)       KIVA              4 0
015600990609     C   27              Z-ADD     KCU(XX)       KBNA              4 0
015700990609     C     end1          tag
015800990609     C                   end
015900990609     C                   SETOFf                                       212223
016000990609     C                   sETOFf                                       242526
016100990609     C                   SETOFf                                       27
016101081002      *
016102081002     C     KE2TAB        KLIST                                                  *TABEL00F
016103081002     C                   KFLD                    codut                           -UTENTE
016104081002     C                   KFLD                    KTBCOD                          -TABELLA
016105081003     C                   KFLD                    ktbkey
016107081003      * Reperimento date ultima pulizia archivi bolle
016108081003      * SEDE
016109120913    1c***                if        simfel=0
016110150323     c                   setoff                                       21
016111150323     c                   if        simfel<>0
016112150323     c                   seton                                        21
016113150323     c                   endif
016114081002     C                   CLEAR                   DS$3
016115081002     C                   MOVEL     '$3'          KTBCOD
016116081003     C                   MOVEL(P)  '1'           ktbkey
016117081003     C     ke2tab        CHAIN     TABEL00F
016118081003    2C                   IF        %found(tabel00f)
016119081002     C                   MOVEL     TBLUNI        DS$3
016120081003    2c                   endif
016121081003      * FILIALE
016122120913   x1c***                else
016123120913     C***                MOVE      '5A'          ktbcod
016124120913     C***                MOVEL(P)  '1'           ktbkey
016125120913     c***                clear                   ds5a
016126120913     C***  Ke2tab        CHAIN     TABEL00F
016127120913    2c***                if        %found(tabel00f)
016128120913     C***                MOVEL     TBLUNI        DS5A
016129120913    2c***                endif
016130120913     C***                CLEAR                   PRIDAT
016131081003      * Reperisco data ultimo utilizzo pgm pulizia bolle
016132120913     C***                MOVE      *BLANKS       $DAT
016133120913     C***                MOVE      *BLANKS       $ERR
016134120913     C***                CALL      'TRUL49C'                            31
016135120913     C***                PARM      'FNLS61R'     $PGM             10
016136120913     C***                PARM                    $DAT              8
016137120913     C***                PARM                    $ERR              1
016138081003      *
016139081003      * Se programma in uso --> data ult.utilizzo = data del giorno
016140081003      * idem se data ultimo utilizzo = 0
016141120913    2C***  $ERR          IFEQ      '1'
016142120913     C***  *IN31         OREQ      '1'
016143120913     c***  $dat          oreq      '00000000'
016144120913     c***                time                    dataiso
016145120913   X2C***                ELSE
016146120913     c***                Move      $dat          wdata
016147120913     C***  *iso          MOVE      wdata         dataiso
016148120913    2C***                ENDIF
016149120913     c***  dataiso       subdur    §5ablp:*days  dataiso
016167120913     C***                move      dataiso       PRIDAT            8 0
016168120913    1c***                endif
016169081002     c
016200990603
016300990603     c                   endsr
016400990603
016500990603     c**********************************************************************
016600990603     c     ctrd01        begsr
016700990603     c                   setoff                                       010203
016800990603     c                   setoff                                       040506
016900990603     c                   setoff                                       070809
017000990603     c                   setoff                                       101112
017100990603     c* controllo data dal
017200990603     c     *dmy          test(d)                 v1cdti                 30
017300990603     c                   if        *in30 = '0'
017400990603     c     *dmy          move      v1cdti        dini_eur
017500990603     c                   else
017600990603     c     *eur          test(d)                 v1cdti                 31
017700990603     c                   if        *in31 = '0'
017800990603     c                   move      v1cdti        dini_eur
017900990603     c                   else
018000990603     c                   seton                                        1128
018100990604     c                   movel     msg(1)        v1zmsg
018200990603     c                   goto      ectd01
018300990603     c                   endif
018400990603     c                   endif
018500990603     c                   move      dini_eur      dini_iso
018600990603     c                   move      dini_eur      v1cdti
018601081002     c* Verifico se per la data sono già trascorsi i giorni di pulizia
018602081002     c                   move      dini_iso      wdat              8 0
018603120913     c***                if        simfel=0
018604081002     c                   if        wdat<=§$3dp0
018605081002     c                   seton                                        1128
018606081002     c                   movel     msg(5)        v1zmsg
018607081002     c                   goto      ectd01
018608081002     c                   endif
018609120913     c***                else
018610120913     c***                if        wdat<=pridat
018611120913     c***                seton                                        1128
018612120913     c***                movel     msg(5)        v1zmsg
018613120913     c***                goto      ectd01
018614120913     c***                endif
018615120913     c***                endif
018700990603
018800990604     c* controllo data al se immessa
018900990604     c                   clear                   dfin_eur
019000990604     c                   clear                   dfin_iso
019100990604     c     v1cdtf        ifgt      *zeros
019200990603     c     *dmy          test(d)                 v1cdtf                 30
019300990603     c                   if        *in30 = '0'
019400990603     c     *dmy          move      v1cdtf        dfin_eur
019500990603     c                   else
019600990603     c     *eur          test(d)                 v1cdtf                 31
019700990603     c                   if        *in31 = '0'
019800990603     c                   move      v1cdtf        dfin_eur
019900990603     c                   else
020000990603     c                   seton                                        1228
020100990604     c                   movel     msg(1)        v1zmsg
020200990603     c                   goto      ectd01
020300990603     c                   endif
020400990603     c                   endif
020500990603     c                   move      dfin_eur      dfin_iso
020600990603     c                   move      dfin_eur      v1cdtf
020700990609     c* errore se data dal > di data al
020800990604     c     dini_iso      ifgt      dfin_iso
020900990604     c                   seton                                        1128
021000990604     c                   movel     msg(2)        v1zmsg
021100990604     c                   goto      ectd01
021200990604     c                   endif
021300990604     c                   endif
021400990604     c
021500990603
021600990603     c* controllo codici clienti
021700990603     c                   do        10            ii                2 0
021800990609     c     vksc(ii)      ifne      *blanks
021900990609     c     vksc(ii)      andne     *zeros
022000990609     C     '?'           SCAN      vksc(ii)                               32
022100990609IF  2C     *in32         ifeq      *on
022200990609     C                   clear                   dsxc
022300990609     C                   z-add     kci           DXCCAP
022400990609     C                   CALL      'XCLIR'
022500990609     C                   PARM                    DSXC
022600990609     C* SCOMPATTA
022700990609     c                   MOVEl     DXCKSC        dsksa                      ALF
022800990609     C                   move      dsksn         w0070             7 0      M
022900990609     C                   move      w0070         vksc(ii)                   M
023000990609     c                   seton                                        29
023100990609     c                   goto      ectd01
023200990609     c                   endif
023300990609     C* CODICE CLIENTE NUMERICO
023400990609     C                   SETOFf                                       32
023500990609     C                   TESTN                   Vksc(ii)             32
023600990609IF  3C     *in32         ifeq      *on
023700990609     C                   move      vksc(ii)      W001              1
023800990609IF  4C     w001          iflt      '0'                                      RA
023900990609     c                   seton                                        28
024000990609     C                   move      '1'           *in(ii)
024100990609     C                   movel     msg(3)        v1zmsg
024200990609     C                   goto      ectd01
024300990609E   4C                   ENDIF
024400990609X   3C                   ELSE                                               MERO
024500990609     c                   seton                                        28
024600990609     C                   move      '1'           *in(ii)
024700990609     C                   movel     msg(3)        v1zmsg
024800990609     C                   goto      ectd01
024900990609E   3C                   ENDIF
025000990609     c*
025100990603     C                   clear                   TIBS69DS
025200990609     c                   move      vksc(ii)      I69kac
025300990603      *
025400990603     C                   CALL      'TIBS69R'
025500990603     C                   PARM                    tibs69DS
025600990603     C                   PARM                    DS_cnaco
025700990603     C                   PARM                    DS_cnind
025800990603     C                   PARM                    DS_cnclp
025900990603     C                   PARM                    DS_fncls
026000990603      *
026100990603IF  2C                   eval      *IN28 = (O69ERR =  '1')
026200990603     C                   eval      wtibs69r = '1'
026300990603     c   28              move      '1'           *in(ii)
026400990604     c   28              movel     msg(3)        v1zmsg
026500990603     c   28              goto      ectd01
026600990603     c                   movel     acorag        dksc(ii)
026700990609     c* errore se codice cliente ripetuto
026800990609     c                   z-add     1             yy                2 0
026900990609     c     vksc(ii)      lookup    vksc(yy)                               30
027000990609     c   30ii            ifne      yy
027100990609     c                   seton                                        28
027200990609     c   28              move      '1'           *in(ii)
027300990609     c   28              movel     msg(4)        v1zmsg
027400990609     c   28              goto      ectd01
027500990609     c                   endif
027600990603     c                   else
027700990603     c                   clear                   dksc(ii)
027800990603     c                   end
027900990603     c                   enddo
028000990603     c     ectd01        endsr
028100990604**
028200990604Data errata                                                                    1
028300990604Data dal > di Data al                                                          2
028400990604Codice cliente errato                                                          3
028500990609Codice cliente ripetuto                                                        4
028600081003Data non elaborabile per mancanza di dati
