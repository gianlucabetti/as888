000100021108     H DECEDIT('0,') DATEDIT(*YMD.)
000200000000      *************************************************************************
000300941220      *  Nome programma:  FNLV45R (era il TRTF57R)
000400000000      *  Descrizione   :  Stampa testi lettere.
000500000000      *  Data creazione:  28 FEB 1994
000600000000      *************************************************************************
000700981113     FAZORG01L  IF   E           K DISK
000800091002     FTABEL00F  IF   E           K DISK
000900091002     FTntbe01l  IF   E           K DISK
001000021022     FFNTXT01L  IF   E           K DISK
001100021022     FFNTXT02L  IF   E           K DISK    RENAME(FNTXT000:FNTXT02)
001200091002     FTFNTC01L  IF   E           K DISK
001300091002     FCNACO00F  IF   E           K DISK    usropn
001400091002     FCNIND00F  IF   E           K DISK    usropn
001500091002     FCNCLP00F  IF   E           K DISK    usropn
001600000000     FTFACO00F  IF   E           K DISK    USROPN
001700000000     F                                     RENAME(CNACO000:TFACO000)
001800091002     FTFIND00F  IF   E           K DISK    USROPN
001900091002     F                                     RENAME(CNIND000:TFIND000)
002000091002     FTFCLP00F  IF   E           K DISK    USROPN
002100091002     F                                     RENAME(CNCLP000:TFCLP000)
002200130802       // -?Anagrafica Commerciali?
002300130731     fAZCMM01L  if   e           k disk
002400130802       // -?Note/Rubrica Commerciali?
002500130802     fAZNTC01L  if   e           k disk    prefix(AZ)
002600021025     FLET198    O    F  132        PRINTER  USROPN  OFLIND(*INOF)
002700021025     FLET017    O    F  124        PRINTER  USROPN  OFLIND(*INOG)
002800021025     FLETNOR    O    F  132        PRINTER  USROPN  OFLIND(*INOE)
002900041029     FLET017M   O    F  124        PRINTER  USROPN  OFLIND(*INOC)
003000041029     FLETNORM   O    F  132        PRINTER  USROPN  OFLIND(*INOD)
003100050908     FLEThtml   O    F  198        PRINTER  USROPN
003200050908     Ffnlv45p   O    e             PRINTER  USROPN  oflind(*IN96)
003300000000      *
003400020927     d PrmFine         S              1
003500020927     d PrmFil          S              3S 0
003600020927     d PrmTpOutQ       S              1
003700020927     d PrmOutQ         S             10
003800020927     d PrmOutQLib      S             10
003900020927     d PrmEsito        S              1
004000041029     d kapl            s                   like(ntcapl)
004100041029     d knk1            s                   like(ntcnk1)
004200041029     d knk2            s                   like(ntcnk2) inz('    ')
004300050908     d ktnt            s                   like(ntctnt)
004400050908     d w45nmc          s                   like(i45nmc)
004500050912     d w45htm          s                   like(i45htm)
004600051007     d clpagex         s                   like(clpage)
004700060608     d $codlin         s                   like(i45lin)
004800060608     d $codpo          s                   like(simfel)
004900060919     d wgiro           S              1
005000060919     d w_noelab        S              1
005100060608
005200060608     d let01           s             25
005300060608     d let02           s             10
005400060608     d let03           s              5
005500060608     d let04           s              5
005600060608     d let05           s             25
005700060608     d let06           s             20
005800060608     d let07           s             55
005900060612     d let08           s             46
006000020927
006100000000      * SCHIERA STAMPA INTESTAZIONE LETTERA
006200000000     D CMD             S              1    DIM(80) CTDATA PERRCD(80)
006300961203     D CM2             S             70    DIM(2) CTDATA PERRCD(1)
006400961203     D SKI             S              1    DIM(140)
006500041102     D SKIM            S              1    DIM(140)
006600050908     D SKIh            S              1    DIM(140)
006700041102     D CMO             S              1    DIM(30) CTDATA PERRCD(30)
006800041102     D CMM             S             70    DIM(2) CTDATA PERRCD(1)
006900050908     D CMMh            S             70    DIM(2) CTDATA PERRCD(1)
007000050909     D CMerr           S             33    DIM(1)  CTDATA PERRCD(1)
007100020626
007200020626     d AzuteDs       e ds                  ExtName(AzUte00F)
007300020626     d DDatiUte      e ds
007400020626     d Tibs34Ds      e ds                  Inz
007500021108      *
007600021108     d Trul31ds      e ds                  INZ
007700091002     d  pog                   10    759  0 dim(250)
007800021108      *
007900020626
008000970225     D KPJBA         E DS
008100060919     d P_numcli              498    502
008200021025     D DFNTXTH       E DS
008300050909     D DS1h          E DS
008400060919     D Dvpo          E DS
008500051012     D og143         E DS
008600021025     D FNLV45DS      E DS
008700120611     d fnlv55ds      e ds
008800091002     D TRUL42DS      E DS
008900041029     D TRUL44DS      E DS
009000050902     D TRTCM1DS      E DS
009100050929     D fnlv54ds      E DS
009200060919     D tibs02ds      E DS
009300021025
009400940315     D                 DS
009500091002     D  ragsoc                 1     48
009600960426     D  RAGIO1                 1     31
009700960426     D  RAGIO2                32     48
009800021025      *
009900981113     D WLBDA8          DS
010000981113     D  G08DAT                 1      8  0
010100981113     D  G08INV                 9     16  0
010200981113     D  G08ERR                17     17
010300981113     D  G08TGI                18     22  0
010400960910      *
010500960910     D                 DS
010600960910     D  WGG                    1      2
010700960910     D  WMM                    3      4
010800960910     D  WAA                    5      8
010900960910     D  WDATA                  1      8
011000091001
011100110909     d StringaSql      s           2048    Varying
011200091002     d xa              s              5i 0 inz(*zeros)
011300091002     d cnacods       e ds                  extname(cnaco00f)
011400091002     d cnindds       e ds                  extname(cnind00f)
011500091002     d cnclpds       e ds                  extname(cnclp00f)
011600091127     d tivisds       e ds                  extname(tivis00f)
011700091002     d $visaco         s               n
011800120611     d wpo             s              3  0
011900021025
012000021025      *----------------------------------------------------------------
012100021025
012200000000     C     *ENTRY        PLIST
012300000000     C                   PARM                    KPJBA
012400021025     C                   PARM                    fnlv45ds
012500020626
012600020626      * reperisco i dati utente
012700020626     c     *dtaara       define    §azute        AzuteDs
012800020626     c     *dtaara       define    §datiute      DDatiUte
012900020626
013000020626     c                   in(E)     *dtaara
013100020626     c                   IF        %Error or RSUT = *blanks
013200020626     c                   call      'TIBS34R'
013300020626     c                   parm                    Tibs34Ds
013400020626     c                   in        *dtaara
013500020626     c                   ENDIF
013600091002
013700091002     c/exec sql
013800091002     c+     set  option  DynUsrPrf = *Owner,
013900091002     c+                  CloSqlCsr = *EndMod
014000091002     c/end-exec
014100020626
014200040804      * SE NON SONO IN SEDE
014300970319     C     SIMFEL        IFGT      *ZEROS
014400021108      * Chiamo il trul31r per reperire schiera po
014500021108      * stesso raggruppamento po utente /stesso AS
014600021108     C                   CLEAR                   TRUL31DS
014700041214     C                   EVAL      I31ABI   = 'RA'
014800021108     C                   EVAL      I31CPO   = DutPou
014900021108     C                   CALL      'TRUL31R'
015000021108     C                   PARM                    KPJBA
015100021108     C                   PARM                    TRUL31DS
015200970319     C                   END
015300021025      *
015400981113     C                   TIME                    W0140            14 0
015500050912     C                   MOVEL     W0140         UTIME             6 0
015600981113     C                   MOVE      W0140         WDTGIO            8 0
015700981113     C                   Z-ADD     WDTGIO        G08DAT
015800981113     C                   MOVEL     *BLANK        G08ERR
015900981113     C                   CALL      'XSRDA8'
016000981113     C                   PARM                    WLBDA8
016100981113     C                   MOVEL     G08INV        DATEU             8 0
016200060919     c*
016300060919     c* reperisco variabili programmi operativi
016400060919     C                   CLEAR                   tibs02ds
016500060919     c                   clear                   dvpo
016600060919     C                   MOVEL     'C'           T02MOD
016700060919     C                   MOVEL     KNSIF         T02SIF
016800060919     C                   MOVEL     'VPO'         T02COD
016900060919     c                   movel(P)  'VPO'         t02ke1
017000060919     C                   CALL      'TIBS02R'
017100060919     C                   PARM                    KPJBA
017200060919     C                   PARM                    tibs02ds
017300060919    2C                   IF        T02ERR = *BLANKS
017400060919     C                   MOVEL     T02uni        dvpo
017500060919    2C                   ENDIF
017600021025
017700000000      *************************************************************************
017800021025
017900961203     C                   MOVEA     CM2(1)        SKI(1)
018000961203     C                   MOVEA     CM2(2)        SKI(71)
018100041102     C                   MOVEA     CMM(1)        SKIM(1)
018200041102     C                   MOVEA     CMM(2)        SKIM(71)
018300050908     C                   MOVEA     CMMh(1)       SKIh(1)
018400050908     C                   MOVEA     CMMh(2)       SKIh(71)
018500981113     C                   MOVEL     I45NCO        W003A             3
018600000920     C                   MOVEA     W003A         CMD(39)
018700000920     C                   MOVEA     W003A         SKI(80)
018800041102     C                   MOVEA     W003A         SKIM(80)
018900110204     C                   MOVE      I45TTX        $TESTO            2
019000021022      *
019100021022      * Aggancio per il tipo testo il file testi record 'H'
019200021022    1C     I45TTX        IFNE      *BLANKS
019300050912     c     i45htm        andeq     *blanks
019400021022     C                   MOVE      'H'           $TpRig
019500060608     c                   eval      $codlin = i45lin
019600060608     c                   eval      $codpo = simfel
019700021022     C     KTxt2         chain     FnTxt02l                           03
019800060608      * se non trovo il testo ed è stata richiesta la stampa in lingua
019900060608      * provo a cercare il testo con il p.o. di sede (000)
020000060608     c                   if        not %found(fntxt02l) and i45lin <> 'I'
020100060608     c                   eval      $codpo = 0
020200060608     c     ktxt2         chain     fntxt02l                           03
020300060608      * se non trovo il testo neanche con il p.o. di sede recupero il
020400060613      * testo in italiano con il p.o.
020500060608     c                   if        not %found(fntxt02l) and i45lin <> 'I'
020600060613     c                   eval      $codpo = simfel
020700060608     c                   eval      $codlin = 'I'
020800060608     c     ktxt2         chain     fntxt02l                           03
020900060613      * se non trovo il testo in italiano riprovo con il p.o. di sede
021000060612     c                   if        not %found(fntxt02l) and i45lin <> 'I'
021100060613     c                   eval      $codpo = 0
021200060612     c     ktxt2         chain     fntxt02l                           03
021300060612     c                   endif
021400060608     c                   endif
021500060608     c                   endif
021600021025     c  n03              movel     TxtRig        DfntxtH
021700021025     c   03              clear                   DfntxtH
021800000000      *
021900000000      * CONTROLLO SUL VALORE DEI FLAG PER LA STAMPA DEI DATI ANAGRAFICI
022000021022     C     §TXSIP        COMP      'S'                                2121
022100021022     C     §TXSIC        COMP      'S'                                2222
022200021022     C     §TXSCC        COMP      'S'                                2323
022300021022     C     §TXSCA        COMP      'S'                                2424
022400021022     C     §TXSTC        COMP      'S'                                2525
022500021022     C     §TXSDC        COMP      'S'                                2626
022600021022     C     §TXSPF        COMP      'S'                                2727
022700021022     C     §TXSNP        COMP      'S'                                2828
022800000000      *
022900000000      * Prima di aprire il file di stampa controllo se compresso o no
023000021022    2C     §TXFTS        IFEQ      'C'
023100000920     C                   MOVEA     '(16,7)'      CMD(25)
023200000920     C                   MOVEA     '(16,7)'      SKI(27)
023300000920     C                   MOVEA     'LET017'      SKI(9)
023400041122     C                   MOVEA     '(16,7)'      SKIM(27)
023500041102     C                   MOVEA     'LET017M'     SKIM(9)
023600021025     C                   eval      *in33 = *on
023700000000     C                   ELSE
023800021025     C                   MOVEA     '(10)  '      CMD(25)
023900021025     C                   MOVEA     '(10)  '      SKI(27)
024000970326     C                   MOVEA     'LETNOR'      SKI(9)
024100041102     C                   MOVEA     '(10)  '      SKIM(27)
024200041102     C                   MOVEA     'LETNORM'     SKIM(9)
024300021025     C                   eval      *in33 = *off
024400940330    2C                   ENDIF
024500000000      *
024600000000      * Eseguo OVRPRTF sul file di stampa
024700000000     C                   Z-ADD     80            LUNG             15 5
024800000000     C                   MOVEL     *BLANKS       COMMAN           80
024900000000     C                   MOVEA     CMD           COMMAN
025000000000     C                   CALL      'QCMDEXC'                            H1
025100000000     C                   PARM                    COMMAN
025200000000     C                   PARM                    LUNG
025300000000     C                   OPEN      LET198
025400961129     C*
025500041029     C* ESEGUO OVRPRTF SUL FILE DI STAMPA PER INVIO TRAMITE FAX
025600981113     C     I45FAX        IFEQ      'S'
025700981113     C     I45AST        ANDNE     'L'
025800050908     C                   EXSR      OVRFAX
025900981113     C     I45IMM        IFEQ      'I'
026000970326     C                   MOVEL     '*'           STAIMM            1
026100970326    3C                   ENDIF
026200970326    2C                   ENDIF
026300970326    1C                   ENDIF
026400021025
026500940330      * Ho 4 strade tra cui scegliere:
026600091127      * + 1 per le trattative
026700981113      *   1)  I45AST='C'  utilizzare i clienti effettivi
026800981113      *   2)  I45AST='V'  utilizzare le visite
026900981113      *   3)  I45AST='P'  utilizzare i clienti potenziali
027000981113      *   3)  I45AST='L'  libero
027100091127      *   5)  I45AST='T'  utilizzare le trattative
027200021025
027300021025     c                   SELECT
027400021025     C                   when      I45AST  = 'L'
027500940330     C                   EXSR      STRAD4
027600021025     C                   when      I45AST  = 'C'
027700000000     C                   EXSR      STRAD1
027800091127     C                   when      I45AST  = 'T'
027900091127     C                   EXSR      STRAD5
028000021025     c                   ENDSL
028100000000      *
028200000000     C                   CLOSE     LET198
028300000920     C   33              CLOSE     LET017
028400970326     C  N33              CLOSE     LETNOR
028500050909     c
028600050909     c                   if        opnerr = '1'
028700050909     c                   write     fines
028800050909     c                   close     fnlv45p
028900050909     c                   endif
029000021025
029100021025     C                   eval      *inLR = *on
029200050929     c*
029300050929     C                   MOVEL     'C'           I54TLA
029400050929     C                   CALL      'FNLV54R'
029500050929     C                   PARM                    fnlv54ds
029600961203      *************************************************************************
029700961203      * ROUTINE PER CREARE SPOOL PER INVIO FAX
029800961203      *************************************************************************
029900961203     C     OVRFAX        BEGSR
030000021025      *
030100021025      * ESEGUO OVRPRTF PER CREARE UNO SPOOL PER LE LETTERE DA INVIARE
030200021025      *   TRAMITE FAX
030300021025      *
030400971205     C* DETERMINO LA OUTQ
030500020927
030600020927     c                   Eval      PrmFine = 'L'
030700020927     c                   Eval      PrmFil = Dutpou
030800020927     c                   Eval      PrmTpOutq = '2'
030900020927     c                   Clear                   PrmOutQ
031000020927     c                   Clear                   PrmOutQLib
031100020927     c                   Clear                   PrmEsito
031200020927     c                   Call      'TRULOUTQ'
031300020927     c                   Parm                    PrmFine
031400020927     c                   Parm                    PrmFil
031500020927     c                   Parm                    PrmTpOutQ
031600020927     c                   Parm                    PrmOutQ
031700020927     c                   Parm                    PrmOutQLib
031800020927     c                   Parm                    PrmEsito
031900020927     c                   If        PrmEsito = '0' and PrmOutq <> *Blanks
032000020927     c                   Movea     PrmOutq       Ski(39)
032100020927     c                   Else
032200020927     c                   Movea     '*SAME     '  Ski(39)
032300020927     c                   EndIf
032400971205     C*
032500961203     C                   Z-ADD     140           LUNG
032600961203     C                   MOVEL     *BLANKS       COMM            140
032700961203     C                   MOVEA     SKI           COMM
032800961203     C                   CALL      'QCMDEXC'                            H1
032900961203     C                   PARM                    COMM
033000961203     C                   PARM                    LUNG
033100961203     C*
033200000920     C   33              OPEN      LET017
033300041028     c   33              move      '1'           opnlet            1
033400970326     C  N33              OPEN      LETNOR
033500041028     c  n33              move      'L'           opnlet
033600961203     C*
033700961203     C                   ENDSR
033800041029      *************************************************************************
033900041029      * ROUTINE PER CREARE SPOOL PER INVIO e-mail
034000041029      *************************************************************************
034100041029     C     OVRmail       BEGSR
034200041029      *
034300041029      * ESEGUO OVRPRTF PER CREARE UNO SPOOL PER LE LETTERE DA INVIARE
034400041029      *   TRAMITE E-mail
034500041029      *
034600041029     c                   select
034700041029     c     opnletm       wheneq    '7'
034800041029     c                   close     let017m
034900041102     c                   movea     'LET017M'     cmo(13)
035000041102     C                   Z-ADD     80            LUNG             15 5
035100041102     C                   MOVEL     *BLANKS       COMMAN           80
035200041102     C                   MOVEA     CMO           COMMAN
035300041102     C                   CALL      'QCMDEXC'                            H1
035400041102     C                   PARM                    COMMAN
035500041102     C                   PARM                    LUNG
035600041029     c     opnletm       wheneq    'N'
035700041029     c                   close     letnorm
035800041102     c                   movea     'LETNORM'     cmo(13)
035900041102     C                   Z-ADD     80            LUNG             15 5
036000041102     C                   MOVEL     *BLANKS       COMMAN           80
036100041102     C                   MOVEA     CMO           COMMAN
036200041102     C                   CALL      'QCMDEXC'                            H1
036300041102     C                   PARM                    COMMAN
036400041102     C                   PARM                    LUNG
036500041029     c                   endsl
036600041029     c                   clear                   opnletm
036700041029     c*
036800041102     c                   clear                   trul44ds
036900041029     c                   eval      d44pgm = 'FNLV45R'
037000041102     c                   z-add     dutpou        d44po
037100041103     c     i45ast        ifne      'L'
037200041029     c                   eval      d44apl = kapl
037300041029     c                   eval      d44nk1 = knk1
037400041029     c                   eval      d44nk2 = knk2
037500041029     c                   eval      d44tnt = ktnt
037600041103     c                   else
037700041103     c                   eval      d44des = i45mai
037800041103     c                   endif
037900050912     c                   eval      §cm1var  = '*OBJM*' + i45obj
038000050912     c                   select
038100091022     c                   when      i45ast = 'T'
038200110204     c                   move      visnrv        w007a
038300091022     c                   eval      §cm1var= %trim(§cm1var) + '-T.'     +
038400091022     c                             w007a
038500050912     c                   when      i45ast = 'C'
038600050912     c                   move      acoksc        w007a             7
038700050912     c                   eval      §cm1var= %trim(§cm1var) + '-C.'      +
038800050912     c                             w007a
038900050912     c                   endsl
039000060313     c                   exsr      RepEmaMit
039100060313     c                   eval      §cm1mitt = w§cm1mitt
039200041029     c                   call      'TRUL44R'
039300041029     c                   parm                    trul44ds
039400050902     c                   parm                    trtcm1ds
039500041102     c     d44err        ifeq      *blanks
039600041102     c* eseguo ovprtf
039700050908     c                   movea     d44outq       skim(39)
039800041102     C                   MOVEL     *BLANKS       COMMM           395
039900050908     C                   MOVEA     SKIm          COMMM
040000041102     c                   eval      commm = %trim(commm) + d44dta
040100041102     c                             + ''')'
040200041102     c                   z-add     395           lungm            15 5
040300041102     C                   CALL      'QCMDEXC'                            H1
040400041102     C                   PARM                    COMMM
040500041102     C                   PARM                    LUNGM
040600041102     c
040700041029     C*
040800041029     C   33              OPEN      LET017M
040900041029     c   33              move      '7'           opnletm           1
041000041029     C  N33              OPEN      LETNORM
041100041029     c  N33              move      'N'           opnletm
041200041102     c                   endif
041300041029     C*
041400041029     C                   ENDSR
041500050908      *************************************************************************
041600050908      * ROUTINE PER CREARE SPOOL PER INVIO testo html
041700050908      *************************************************************************
041800050908     C     OVRhtml       BEGSR
041900050908      *
042000050908      * ESEGUO OVRPRTF PER CREARE UNO SPOOL PER LE LETTERE DA INVIARE
042100050908      *   TRAMITE E-mail HTML
042200050908      *
042300050908     c* Eseguo dltovr
042400050908     c                   if        opnlethtm <> ' '
042500050908     c                   close     lethtml
042600050908     c                   movea     'LETHTML'     cmo(13)
042700050908     C                   Z-ADD     80            LUNG             15 5
042800050908     C                   MOVEL     *BLANKS       COMMAN           80
042900050908     C                   MOVEA     CMO           COMMAN
043000050908     C                   CALL      'QCMDEXC'                            H1
043100050908     C                   PARM                    COMMAN
043200050908     C                   PARM                    LUNG
043300050908     c                   clear                   opnlethtm
043400050908     c                   endif
043500050908     c*
043600050908     c                   clear                   trul44ds
043700050912     c                   eval      d44pgm = 'FNLV45RHTM'
043800050908     c                   z-add     dutpou        d44po
043900050908     c     i45emc        ifeq      *blanks
044000050908     c                   eval      d44apl = kapl
044100050908     c                   eval      d44nk1 = knk1
044200050908     c                   eval      d44nk2 = knk2
044300050908     c                   eval      d44tnt = '06'
044400050908     c                   else
044500050908     c                   eval      d44des = i45emc
044600050908     c                   endif
044700050912     c                   eval      §cm1var= '*OBJM*' + %trim(i45obj)
044800050909     c                   select
044900091022     c                   when      i45ast = 'T'
045000110204     c                   move      visnrv        w007a
045100091022     c                   eval      §cm1var= %trim(§cm1var) + '-T.'     +
045200091022     c                             w007a
045300050909     c                   when      i45ast = 'C'
045400050909     c                   move      acoksc        w007a             7
045500050912     c                   eval      §cm1var= %trim(§cm1var) + '-C.'      +
045600050912     c                             w007a
045700050909     c                   endsl
045800050912     c                   eval      §cm1var = %trim(§cm1var) + '§*EMLOFT*' +
045900050912     c                             $testo + '.OFT'
046000060313     c                   exsr      RepEmaMit
046100060313     c                   eval      §cm1mitt = w§cm1mitt
046200050912     c
046300050909     c
046400050908     c                   call      'TRUL44R'
046500050908     c                   parm                    trul44ds
046600050908     c                   parm                    trtcm1ds
046700050908     c     d44err        ifeq      *blanks
046800050908     c* eseguo ovprtf
046900050908     c                   movea     d44outq       skih(39)
047000050908     C                   MOVEL     *BLANKS       COMMM           395
047100050908     C                   MOVEA     SKIh          COMMM
047200050908     c                   eval      commm = %trim(commm) + d44dta
047300050908     c                             + ''')'
047400050908     c                   z-add     395           lungm            15 5
047500050908     C                   CALL      'QCMDEXC'                            H1
047600050908     C                   PARM                    COMMM
047700050908     C                   PARM                    LUNGM
047800050908     c
047900050908     C*
048000050908     C                   OPEN      LEThtml
048100050908     c                   move      '1'           opnlethtm         1
048200050908     c                   endif
048300050908     C*
048400050908     C                   ENDSR
048500000000      *************************************************************************
048600000000      * STRADA 1:  clienti effettivi
048700000000      *************************************************************************
048800000000     C     STRAD1        BEGSR
048900000000      *
049000091002      * preparo la stringa SQL
049100091002     c                   exsr      sr_sql1
049200091001
049300060919     c                   eval      wgiro='1'
049400060919     c                   clear                   wcontcli          9 0
049500060919     c     I_strad1      tag
049600091002
049700091002     C                   MOVEL     I45KCC        NOTNK1
049800981113     C                   MOVEL     I45KCC        KEY1C
049900000000     C                   MOVEL     '05'          NOTTNT
050000000000     C                   MOVEL     'C'           APPL
050100000000     C                   MOVEL     'C'           APPLC
050200000000     C                   MOVEL     *BLANKS       NOTNK2
050300091002
050400091002      * apro il cursore da leggere
050500091002     C/EXEC SQL
050600091002     C+ Open C1
050700091002     C/END-EXEC
050800091002
050900091002     c                   Do        *hival
051000091002
051100091002      * leggo il cursore
051200091002     C/EXEC SQL
051300091002     C+ Fetch next from C1 into :cnacods, :cnindds, :cnclpds
051400091002     C/END-EXEC
051500091002
051600091002     c                   if        sqlcod = 100
051700091002     c                   leave
051800091002     c                   endif
051900091002     c                   if        sqlcod < 0
052000091006     c                   seton                                        H1
052100091002     c                   leavesr
052200091002     c                   endif
052300000000      *
052400060919     c* se sono nel primo giro devo solo contare il numero dei clienti
052500060919     c* da elaborare quindi torno a leggere
052600060919     c                   if        wgiro='1'
052700060919     c                   add       1             wcontcli
052800091002     c                   iter
052900060919     c                   endif
053000000000      *
053100000000      * Il codice filiale e' nei primi tre caratteri del cd.cliente
053200000000     C                   MOVEL     ACOKSC        $FILIA            3 0
053300940315      *
053400091002     c                   movel     acorag        ragsoc
053500960426     C                   MOVEL     ACORAG        WRAG             46
053600940331     C                   MOVEL     ACOKSC        STACOD           11
053700950201     C                   MOVEL     INDCAE        WCAP
053800950201     C                   MOVEL     INDSTA        WNAR              3
053900000000      *
054000041029     C* CONTROLLO SE INVIO TRAMITE E-MAIL/FAX OPPURE NO
054100041029     c                   movel     'C'           kapl
054200041029     c                   movel     acokcc        knk1
054300041029     c                   move      acoksc        knk1
054400050912     c                   movel     i45htm        w45htm
054500060313     c                   movel     clpage        keypo
054600060313     c                   move      clpage        clpagex
054700961203     C                   EXSR      CTRFAX
054800961129      *
054900000000      * Stampo la lettera per il cliente
055000050912     c                   if        i45htm = *blanks
055100060919     c                   if        w_noelab = ' '
055200000000     C                   EXSR      TESLET
055300000000     C                   EXSR      CORLET
055400000000     C                   EXSR      FINLET
055500060919     c                   endif
055600050908     c                   else
055700050908     c                   if        d44err = *blanks
055800050908     c                   exsr      prthtml
055900050908     c                   else
056000050908     c                   exsr      s_errhtml
056100050908     c                   endif
056200050908     c                   endif
056300000000      *
056400000000      * Pulisco i campi anagrafici
056500000000     C                   MOVE      *BLANKS       ACORAG
056600960426     C                   MOVE      *BLANKS       WRAG
056700091002     C                   MOVE      *BLANKS       ragsoc
056800000000     C                   MOVE      *BLANKS       INDVIA
056900000000     C                   MOVE      *BLANKS       INDCIT
057000000000     C                   MOVE      *BLANKS       INDPRV
057100941230     C                   CLEAR                   WCAP
057200950201     C                   CLEAR                   WNAR
057300941230     C                   CLEAR                   INDCAE
057400000000     C                   Z-ADD     *ZEROS        CLPAGE
057500000000      *
057600000000     C                   ENDDO
057700091002
057800091002     C/EXEC SQL
057900091002     C+ CLOSE C1
058000091002     C/END-EXEC
058100060919     c*
058200060919     c                   if        wgiro='1'
058300060919     c                   if        wcontcli <=§vponclimx
058400060919     c                   eval      wgiro='2'
058500060919     c                   goto      I_strad1
058600060919     c                   else
058700060919     c* passo al chiamante il numero dei clienti elaborabili per
058800060919     c* l'emissione del msg di errore
058900060919     c                   if        wcontcli >= 99999
059000060919     c                   move      '99999'       p_numcli
059100060919     c                   else
059200060919     c                   move      wcontcli      p_numcli
059300060919     c                   endif
059400060919     c                   endif
059500060919     c                   else
059600060919     c                   clear                   p_numcli
059700060919     c                   endif
059800000000      *
059900000000     C                   ENDSR
060000940330      *************************************************************************
060100940330      * STRADA 4:  libero
060200940330      *************************************************************************
060300940330     C     STRAD4        BEGSR
060400940330      *
060500940330      *
060600050908     c* TESTO TRADIZIONALE
060700050912     c                   if        i45htm = *blanks
060800940330      * Imposto i campi anagrafici
060900981113     C                   Z-ADD     I45APP        $FILIA
061000091002     C                   MOVEL(P)  I45RAG        acorag
061100091002     C                   MOVEL(P)  I45RAG        ragsoc
061200981113     C                   MOVEL(P)  I45RAG        WRAG
061300981113     C                   MOVEL     I45VIA        INDVIA
061400981113     C                   MOVEL     I45CIT        INDCIT
061500981113     C                   MOVEL     I45PRO        INDPRV
061600981113     C                   MOVEL     I45CAP        WCAP
061700981113     C                   MOVEL     I45NAR        WNAR
061800940330     C                   CLEAR                   STACOD
061900970324     C*
062000041103     c                   select
062100041103     c     i45fax        wheneq    'S'
062200041103     c     i45mai        andne     *blanks
062300060919     c     i45fax        oreq      'M'
062400041103     c                   exsr      ovrmail
062500041103     c                   seton                                        09
062600041103     C     I45FAX        wheneq    'S'
062700970326     C                   EXSR      OVRFAX
062800970324     C                   SETON                                        08
062900981113     C                   MOVEL     I45NFX        INDTLF
063000981113     C     I45IMM        IFEQ      'I'
063100970326     C                   MOVEL     '*'           STAIMM
063200970326     C                   ENDIF
063300041103     C                   ENDsl
063400940330      *
063500940330      * Stampo la lettera per il cliente
063600940330     C                   EXSR      TESLET
063700940330     C                   EXSR      CORLET
063800940330     C                   EXSR      FINLET
063900050908     c* TESTO HTML
064000050908     c                   else
064100050912     c                   movel     i45htm        w45htm
064200050908     c                   exsr      ovrhtml
064300050908     c                   clear                   w45nmc
064400050909     c                   eval      w45nmc = i45nmc
064500050929     c                   z-add     dutpou        keypo
064600050908     c                   exsr      prthtml
064700050908     c
064800050908     c                   endif
064900940330      *
065000940330     C                   ENDSR
065100091127
065200091127      *-----------------------------------------------------------------------*
065300091127      * STRADA 5:  Trattative
065400091127      *-----------------------------------------------------------------------*
065500091127     c     Strad5        Begsr
065600091127
065700091127      * controllo se sql su visite e anagrafiche provvisorie
065800091127     c                   if        i45sct <> 0 or i45clv <> *blanks or
065900091127     c                             i45pro <> *blanks or
066000091127     c                             i45cap <> *blanks or
066100091127     c                             (i45nar <> '999' and i45nar <> '   ')
066200091127     c                   eval      $visaco = *on
066300091127     c                   else
066400091127     c                   eval      $visaco = *off
066500091127     c                   endif
066600091127
066700091127      * preparo la stringa SQL
066800091127     c                   exsr      sr_sql5
066900091127
067000091127      * Apro i file interessati
067100091127     c                   if        not $visaco
067200091127     C                   OPEN      cnaCO00F
067300091127     C                   OPEN      cnIND00F
067400091127     C                   OPEN      cnCLP00F
067500091127     C                   OPEN      TFACO00F
067600091127     C                   OPEN      TFIND00F
067700091127     C                   OPEN      TFCLP00F
067800091127     c                   endif
067900091127
068000091127     c                   eval      wgiro='1'
068100091127     c                   clear                   wcontcli          9 0
068200091127     c     I_strad5      tag
068300091127      *
068400091127     C                   MOVEL     I45KCC        NOTNK1
068500091127     C                   MOVEL     I45KCC        KEY1C
068600091127     C                   MOVEL     '05'          NOTTNT
068700091127     C                   MOVEL     i45ast        APPL
068800091127     C                   MOVEL     'C'           APPLC
068900091127     C                   MOVEL     *BLANKS       NOTNK2
069000091127
069100091127      * apro il cursore da leggere
069200091127     c                   select
069300091127     c                   when      $visaco and i45ast = 'T'
069400091127     C/EXEC SQL
069500091127     C+ Open C5a
069600091127     C/END-EXEC
069700091127     c                   when      not $visaco and i45ast = 'T'
069800091127     C/EXEC SQL
069900091127     C+ Open C5b
070000091127     C/END-EXEC
070100091127     c                   endsl
070200091127
070300091127     c                   Do        *hival
070400091127
070500091127      * leggo il cursore
070600091127     c                   select
070700091127     c                   when      $visaco and i45ast = 'T'
070800091127     C/EXEC SQL
070900091127     C+ Fetch next from C5a into :tivisds,
071000091127     C+                          :cnacods, :cnindds, :cnclpds
071100091127     C+
071200091127     C/END-EXEC
071300091127     c                   when      not $visaco and i45ast = 'T'
071400091127     C/EXEC SQL
071500091127     C+ Fetch next from C5b into :tivisds
071600091127     C/END-EXEC
071700091127     c                   endsl
071800091127
071900091127     c                   if        sqlcod = 100
072000091127     c                   leave
072100091127     c                   endif
072200091127     c                   if        sqlcod < 0
072300091127     c                   seton                                        lr
072400091127     c                   leavesr
072500091127     c                   endif
072600091127
072700091127     c* se sono nel primo giro devo solo contare il numero dei clienti
072800091127     c* da elaborare quindi torno a leggere
072900091127     c                   if        wgiro='1'
073000091127     c                   add       1             wcontcli
073100091127     c                   iter
073200091127     c                   endif
073300091127      *
073400091127      * Decodifico subito il codice cliente dall'anagrafica visite/offerte
073500091127     C                   Z-ADD     1             ACOKUT
073600091127     C                   Z-ADD     I45KCC        ACOKCC
073700110204     C                   Z-ADD     visnrv        ACOKSC
073800091127     c* preparo anche la chiave per ricerca nota06
073900091127     c                   movel     i45ast        kapl
074000091127     c                   movel     acokcc        knk1
074100091127     c                   move      acoksc        knk1
074200091127     C     CNACOK        CHAIN     TFACO000                           99
074300091127     C  N99CNACOK        CHAIN     TFIND000                           99
074400091127     C  N99CNACOK        CHAIN     TFCLP000                           99
074500091127      *
074600091127      * Se non trovato cerco in quella effettiva
074700091127     C   99              DO
074800110204     C                   Z-ADD     visksc        ACOKSC
074900091127     C     CNACOK        CHAIN     CNACO000                           99
075000091127     C  N99CNACOK        CHAIN     CNIND000                           99
075100091127     C  N99CNACOK        CHAIN     CNCLP000                           99
075200091127     C                   ENDDO
075300091127
075400091127      *
075500091127      * Rilevo il codice filiale (primi tre caratteri del codice ...)
075600110204     C     visksc        IFLE      0
075700110204     C                   MOVEL     viscmm        $FILIA
075800091127     C                   MOVEL     *BLANKS       STACOD
075900091127     C                   ELSE
076000110204     C                   MOVEL     visksc        $FILIA
076100110204     C                   MOVEL     visksc        STACOD           11
076200091127     C                   ENDIF
076300091127     C*
076400110204     C                   Z-ADD     viscmm        CLPAGE
076500091127     C                   MOVEL     ACORAG        ragsoc
076600091127     C                   MOVEL     ACORAG        WRAG
076700091127     C                   MOVEL     INDCAE        WCAP
076800091127     C                   MOVEL     INDSTA        WNAR
076900091127      *
077000091127     C* CONTROLLO SE INVIO TRAMITE E-MAIL/FAX OPPURE NO
077100091127     c                   movel     i45htm        w45htm
077200110204     c                   movel     viscmm        keypo             3 0
077300110204     c                   move      viscmm        clpagex
077400091127     C                   EXSR      CTRFAX
077500091127      *
077600091127      * Stampo la lettera per il cliente
077700091127     c                   if        i45htm = *blanks
077800091127     c                   if        w_noelab =' '
077900091127     C                   EXSR      TESLET
078000091127     C                   EXSR      CORLET
078100091127     C                   EXSR      FINLET
078200091127     c                   endif
078300091127     c                   else
078400091127     c                   if        d44err = *blanks
078500091127     c                   exsr      prthtml
078600091127     c                   else
078700091127     c                   exsr      s_errhtml
078800091127     c                   endif
078900091127     c                   endif
079000091127      *
079100091127      * Pulisco i campi anagrafici
079200091127     C                   MOVE      *BLANKS       ACORAG
079300091127     C                   MOVE      *BLANKS       WRAG
079400091127     C                   MOVE      *BLANKS       ragsoc
079500091127     C                   MOVE      *BLANKS       INDVIA
079600091127     C                   MOVE      *BLANKS       INDCIT
079700091127     C                   MOVE      *BLANKS       INDPRV
079800091127     C                   CLEAR                   INDCAE
079900091127     C                   CLEAR                   WCAP
080000091127     C                   CLEAR                   WNAR
080100091127     C                   MOVE      *ZEROS        CLPAGE
080200091127      *
080300091127     C                   ENDDO
080400091127
080500091127     c                   select
080600091127     c                   when      $visaco and i45ast = 'T'
080700091127     C/EXEC SQL
080800091127     C+ CLOSE C5a
080900091127     C/END-EXEC
081000091127     c                   when      not $visaco and i45ast = 'T'
081100091127     C/EXEC SQL
081200091127     C+ CLOSE C5b
081300091127     C/END-EXEC
081400091127     c                   endsl
081500091127
081600091127     c                   if        wgiro='1'
081700091127     c                   if        wcontcli <=§vponclimx
081800091127     c                   eval      wgiro='2'
081900091127     c                   goto      I_strad5
082000091127     c                   else
082100091127     c* passo al chiamante il numero dei clienti elaborabili per
082200091127     c* l'emissione del msg di errore
082300091127     c                   if        wcontcli >= 99999
082400091127     c                   move      '99999'       p_numcli
082500091127     c                   else
082600091127     c                   move      wcontcli      p_numcli
082700091127     c                   endif
082800091127     c                   endif
082900091127     c                   else
083000091127     c                   clear                   p_numcli
083100091127     c                   endif
083200091127      *
083300091127     C                   ENDSR
083400091002
083500091001      *-----------------------------------------------------------------------*
083600091002      * Stringa sql per clienti
083700091001      *-----------------------------------------------------------------------*
083800091002     c     sr_sql1       begsr
083900091002
084000091002     c                   clear                   StringaSql
084100091001
084200091001     c                   eval      StringaSql =
084300091001     c                             'select * -
084400091002     c                             from cnaco00f, cnind00f, cnclp00f -
084500091002     c                             where acokut = indkut and acokut = -
084600091002     c                             clpkut and acokcc = indkcc and -
084700091002     c                             acokcc = clpkcc and acoksc = -
084800091002     c                             indksc and acoksc = clpksc and -
084900091002     c                             acokut = 1 and acokcc = 151 and -
085000091006     c                             acoflg <> ''A'' '
085100091002      * è richiesto un solo cliente
085200091002     c                   if        i45ksc <> 0
085300091002     c                   eval      StringaSql = %trim(StringaSql) +
085400091006     c                             ' and acoksc = ' +
085500091002     c                             %editc(i45ksc:'X')
085600091002     c                   goto      $dopo1
085700091002     c                   endif
085800091002
085900091002      * più clienti
086000091002     c                   eval      StringaSql = %trim(StringaSql) +
086100091006     c                             ' and substr(digits(acoksc), 4, 4) <> -
086200091002     c                              ''88888'' and substr(digits(-
086300091002     c                              acoksc), 4, 4) <> ''9999'' '
086400091001
086500091002      * se sono in filiale escludo i clienti non gestibili
086600091002      * se non richiesta 1 sola filiale
086700091002     c                   if        simfel <> 0 and i45app = 0
086800091002     c                   eval      StringaSql = %trim(StringaSql) +
086900091002     c                             ' and substr(digits(acoksc), 1, 3) -
087000091002     c                             in ('
087100091002     c                   do        250           xa
087200091002     c                   if        pog(xa) = *zeros
087300091002     c                   leave
087400091002     c                   endif
087500091002     c                   eval      StringaSql =
087600091001     c                             %trim(StringaSql) +  ' ' +
087700091002     c                             %editc(pog(xa):'X') + ','
087800091002     c                   enddo
087900091002     c                   eval      StringaSql =
088000091002     c                             %trim(StringaSql) + ')'
088100091002     c                   endif
088200091002
088300091002      * è richiesto un commerciale
088400091002     c                   if        i45cmm <> 0
088500091002     c                   eval      StringaSql =
088600091002     c                             %trim(StringaSql) +
088700091002     c                             ' and clpage = ' +
088800091002     c                             %editc(i45cmm:'X')
088900091002     c                   endif
089000091002      * è richiesta la categoria
089100091002     c                   if        i45sct <> 0
089200091002     c                   eval      StringaSql =
089300091002     c                             %trim(StringaSql) +
089400091002     c                             ' and acoitc = ' +
089500091002     c                             %editc(i45sct:'X')
089600091002     c                   endif
089700091002      * è richiesto il codice importanza clienti
089800091002     c                   if        i45clv <> *blanks
089900091002     c                   eval      StringaSql =
090000091002     c                             %trim(StringaSql) +
090100091002     c                             ' and clpclv = ' + '''' +
090200091002     c                              i45clv + ''''
090300091002     c                   endif
090400091002      * è richiesta una filiale
090500091002     c                   if        i45app <> 0
090600091002     c                   eval      StringaSql =
090700091002     c                             %trim(StringaSql) + ' and -
090800091002     c                             substr(digits(acoksc), 1, 3) = '
090900091002     c                             + %editc(i45app:'X')
091000091002     c                   endif
091100091002      * è richiesta una provincia
091200091002     c                   if        i45pro <> *blanks
091300091002     c                   eval      StringaSql =
091400091002     c                             %trim(StringaSql) +
091500091002     c                             ' and indprv = ' +
091600091002     c                             '''' + i45pro + ''''
091700091002     c                   endif
091800091002      * è richiesto un CAP
091900091002     c                   if        i45cap <> *blanks
092000091002     c                   eval      StringaSql =
092100091002     c                             %trim(StringaSql) +
092200091002     c                             ' and indcae = ' +
092300091002     c                             '''' + i45cap + ''''
092400091002     c                   endif
092500091002      * è richiesta una nazione
092600091002     c                   if        i45nar <> '999'
092700091002     c                   eval      StringaSql =
092800091002     c                             %trim(StringaSql) +
092900091002     c                             ' and indsta = ' +
093000091002     c                             '''' + i45nar + ''''
093100091002     c                   endif
093200091002      * solo clienti bloccati
093300091002     c                   if        i45abl = 'B'
093400091002     c                   eval      StringaSql =
093500091002     c                             %trim(StringaSql) +
093600091002     c                             ' and acoabl <> '' '''
093700091002     c                   endif
093800091002      * solo clienti NON bloccati
093900091002     c                   if        i45abl = 'N'
094000091002     c                   eval      StringaSql =
094100091002     c                             %trim(StringaSql) +
094200091002     c                             ' and acoabl = '' '''
094300091002     c                   endif
094400091002
094500091002     c     $dopo1        tag
094600091002
094700091001     c                   Eval      StringaSql =
094800091002     c                             %trim(StringaSql) + ' for read only'
094900091001
095000091001     C/EXEC SQL
095100091001     C+ prepare S1 from :StringaSql
095200091001     C/END-EXEC
095300091001
095400091001     C/EXEC SQL
095500091001     C+ declare C1 cursor for S1
095600091001     C/END-EXEC
095700091001
095800091001     c                   endsr
095900091127
096000091127      *-----------------------------------------------------------------------*
096100091127      * Stringa sql per trattative
096200091127      *-----------------------------------------------------------------------*
096300091127     c     sr_sql5       begsr
096400091127
096500091127     c                   clear                   StringaSql
096600091127
096700091127      * sql con trattative e anagragriche
096800091127     c                   if        $visaco
096900091127
097000091127     c                   eval      StringaSql =
097100091127     c                             'select * -
097200091127     c                              from tivis00f, tfaco00f, tfind00f, -
097300091127     c                              tfclp00f -
097400091127     c                              where visnrv = acoksc and -
097500091127     c                              acokut = indkut and acokut = -
097600091127     c                              clpkut and acokcc = indkcc and -
097700091127     c                              acokcc = clpkcc and acoksc = -
097800091127     c                              indksc and acoksc = clpksc and -
097900091127     c                              acokut = 1 and -
098000091127     c                              acoflg <> ''A'' and visdch = 0 -
098100091127     c                              and visksc = 0 '
098200091127      * è richiesta una sola visita
098300091127     c                   if        i45nrv <> 0
098400091127     c                   eval      StringaSql = %trim(StringaSql) +
098500091127     c                             ' and visnrv = ' +
098600091127     c                             %editc(i45nrv:'X')
098700091127     c                   goto      $dopo5a
098800091127     c                   endif
098900091127
099000091127      * più visite
099100091127      * è richiesto un commerciale
099200091127     c                   if        i45cmm <> 0
099300091127     c                   eval      StringaSql =
099400091127     c                             %trim(StringaSql) +
099500091127     c                             ' and viscmm = ' +
099600091127     c                             %editc(i45cmm:'X')
099700091127     c                   endif
099800091127      * è richiesto il tipo visita
099900091127     c                   if        i45tpv <> *blanks
100000091127     c                   eval      StringaSql =
100100091127     c                             %trim(StringaSql) +
100200091127     c                             ' and vistpv = ' +
100300091127     c                             i45tpv
100400091127     c                   endif
100500091127      * è richiesta una filiale
100600091127     c                   if        i45app <> 0
100700091127     c                   eval      StringaSql =
100800091127     c                             %trim(StringaSql) + ' and -
100900091127     c                             substr(digits(viscmm), 1, 3) = '
101000091127     c                             + %editc(i45app:'X')
101100091127     c                   endif
101200091127      * è richiesta la categoria
101300091127     c                   if        i45sct <> 0
101400091127     c                   eval      StringaSql =
101500091127     c                             %trim(StringaSql) +
101600091127     c                             ' and acoitc = ' +
101700091127     c                             %editc(i45sct:'X')
101800091127     c                   endif
101900091127      * è richiesto il codice importanza clienti
102000091127     c                   if        i45clv <> *blanks
102100091127     c                   eval      StringaSql =
102200091127     c                             %trim(StringaSql) +
102300091127     c                             ' and clpclv = ' + '''' +
102400091127     c                              i45clv + ''''
102500091127     c                   endif
102600091127      * è richiesta una provincia
102700091127     c                   if        i45pro <> *blanks
102800091127     c                   eval      StringaSql =
102900091127     c                             %trim(StringaSql) +
103000091127     c                             ' and indprv = ' +
103100091127     c                             '''' + i45pro + ''''
103200091127     c                   endif
103300091127      * è richiesto un CAP
103400091127     c                   if        i45cap <> *blanks
103500091127     c                   eval      StringaSql =
103600091127     c                             %trim(StringaSql) +
103700091127     c                             ' and indcae = ' +
103800091127     c                             '''' + i45cap + ''''
103900091127     c                   endif
104000091127      * è richiesta una nazione
104100091127     c                   if        i45nar <> '999'
104200091127     c                   eval      StringaSql =
104300091127     c                             %trim(StringaSql) +
104400091127     c                             ' and indsta = ' +
104500091127     c                             '''' + i45nar + ''''
104600091127     c                   endif
104700091127
104800091127     c     $dopo5a       tag
104900091127
105000091127     c                   Eval      StringaSql =
105100091127     c                             %trim(StringaSql) + ' for read only'
105200091127
105300091127     C/EXEC SQL
105400091127     C+ prepare S5a from :StringaSql
105500091127     C/END-EXEC
105600091127
105700091127     C/EXEC SQL
105800091127     C+ declare C5a cursor for S5a
105900091127     C/END-EXEC
106000091127
106100091127     c                   endif
106200091127
106300091127      * sql solo trattative
106400091127     c                   if        not $visaco
106500091127
106600091127     c                   eval      StringaSql =
106700091127     c                             'select * -
106800091127     c                              from tivis00f where visdch = 0'
106900091127      * è richiesta una sola visita
107000091127     c                   if        i45nrv <> 0
107100091127     c                   eval      StringaSql = %trim(StringaSql) +
107200091127     c                             ' and visnrv = ' +
107300091127     c                             %editc(i45nrv:'X')
107400091127     c                   goto      $dopo5b
107500091127     c                   endif
107600091127
107700091127      * più visite
107800091127      * è richiesto un commerciale
107900091127     c                   if        i45cmm <> 0
108000091127     c                   eval      StringaSql =
108100091127     c                             %trim(StringaSql) +
108200091127     c                             ' and viscmm = ' +
108300091127     c                             %editc(i45cmm:'X')
108400091127     c                   endif
108500091127      * è richiesto il tipo visita
108600091127     c                   if        i45tpv <> *blanks
108700091127     c                   eval      StringaSql =
108800091127     c                             %trim(StringaSql) +
108900091127     c                             ' and vistpv = ' +
109000091127     c                             i45tpv
109100091127     c                   endif
109200091127      * è richiesta una filiale
109300091127     c                   if        i45app <> 0
109400091127     c                   eval      StringaSql =
109500091127     c                             %trim(StringaSql) + ' and -
109600091127     c                             substr(digits(viscmm), 1, 3) = '
109700091127     c                             + %editc(i45app:'X')
109800091127     c                   endif
109900091127
110000091127     c     $dopo5b       tag
110100091127
110200091127     c                   Eval      StringaSql =
110300091127     c                             %trim(StringaSql) + ' for read only'
110400091127
110500091127     C/EXEC SQL
110600091127     C+ prepare S5b from :StringaSql
110700091127     C/END-EXEC
110800091127
110900091127     C/EXEC SQL
111000091127     C+ declare C5b cursor for S5b
111100091127     C/END-EXEC
111200091127
111300091127     c                   endif
111400091127
111500091127     c                   endsr
111600091001
111700961203      *************************************************************************
111800041029      * ROUTINE DI CONTROLLO SE INVIO TRAMITE E-MAIL/FAX OPPURE NO
111900961203      *************************************************************************
112000961203     C     CTRFAX        BEGSR
112100050912     c     w45htm        ifeq      *blanks
112200041029     C                   SETOFF                                       0809
112300060919     c                   eval      w_noelab=' '
112400041102    1c     i45fax        ifeq      'S'
112500060919     c     i45fax        oreq      'M'
112600041029     c* verifico se esiste indirizzo e-mail
112700050908     c                   movel     '06'          ktnt
112800050908     c     kntc          chain     tfntc01l
112900060919     c* se visita e non trovata nota per numero visita la cerco con cliente
113000060919     c* se presente
113100110204     c                   if        i45ast = 'T' and
113200091022     c                             (not %Found(tfntc01l) or
113300060919     c                             ntcrnt=*blanks) and visksc>0
113400060919     c                   movel     'C'           kapl
113500060919     c                   movel     i45kcc        knk1
113600060919     c                   move      visksc        knk1
113700060919     c     kntc          chain     tfntc01l
113800060919     c                   endif
113900041102    2c                   if        %found(tfntc01l)
114000041029     c                             and ntcrnt <> *blanks
114100041029     c* eseguo override
114200041029     c                   exsr      ovrmail
114300041102    3c     d44err        ifeq      *blanks
114400041102     c                   seton                                        09
114500041102    3c                   endif
114600091127    2c                   endif
114700060919    1c                   endif
114800060919    1c                   select
114900060919     c                   when      i45fax='S'
115000041102    2c     *in09         ifeq      *off
115100041029     c* indirizzo e-mail non trovato o campo vuoto verifico numero fax
115200041102    3c                   if        indtlf <> *blanks
115300961203     C*
115400961203     C* SE INVIO TRAMITE FAX CONTROLLO VALIDITA' NUMERO FAX
115500050908     C                   CLEAR                   TRUL42DS
115600961203     C                   MOVEL     INDTLF        D42FAX
115700961203     C                   CALL      'TRUL42R'
115800021025     C                   PARM                    TRUL42DS
115900961203     C* 08 ON  - FAX VALIDO: STAMPO NELLO SPOOL PER INVIO FAX
116000041102    4C     D42ERR        IFEQ      ' '
116100961203     C                   SETON                                        08
116200041102    4C                   ENDIF
116300041102    3C                   ENDIF
116400041102    2C                   ENDIF
116500060919     c                   when      i45fax='M'  and *in09=*off
116600060919     c                   eval      W_noelab='1'
116700060919    1C                   ENDsl
116800050908     c                   else
116900050908     c                   exsr      ovrhtml
117000050908     c* se ho trovato e-mail cerco anche nominativo cliente
117100050908     c                   move      i45nmc        w45nmc
117200050908     c                   if        i45nmc = *blanks and d44err=*blanks
117300050908     c                   movel     '05'          ktnt
117400050908     c     kntc          chain     tfntc01l
117500050908     c                   if        %found(tfntc01l)
117600050908     c                             and ntcrnt <> *blanks
117700050908     c                   eval      w45nmc = ntcrnt
117800050908     c                   else
117900050909     c                   eval      w45nmc = 'Responsabile Trasporti'
118000050908     c                   endif
118100050908     c                   endif
118200050908     c                   endif
118300961203     C*
118400961203     C                   ENDSR
118500000000      *************************************************************************
118600000000      * INTESTAZIONE LETTERA
118700000000      *************************************************************************
118800000000     C     TESLET        BEGSR
118900000000      *
119000000000      * Inizializzo il numero delle pagine
119100000000     C                   Z-ADD     1             $PAGIN            4 0
119200000000      *
119300000000      * DECODIFICA DATI ANAGRAFICI
119400000000     C                   EXSR      DECANA
119500940330      *
119600940330      * Compongo la localita'
119700950201     C     WNAR          IFEQ      *BLANKS
119800950201     C* SE NAZIONE E' *BLANK=ITALIA NON LA STAMPO QUINDI STAMPO IL
119900950201     C*   CAP + LOCALITA' + PROV. SU UNA SOLA RIGA
120000960426     C     *IN33         IFEQ      *ON
120100960426     C* 33 ON  - TESTO IN COMPRESSO
120200960426     C                   MOVEL     INDCIT        $COMOD           30
120300960426     C                   MOVE      *BLANKS       LOCALP           34
120400940330     C     $COMOD        CAT       INDPRV:2      LOCALP
120500960426     C                   ELSE
120600960426     C* 33 OFF - TESTO IN NORMALE
120700960426     C                   MOVEL     WCAP          WCAP5             5
120800960426     C                   MOVEL     INDCIT        $COM21           21
120900960426     C                   MOVE      *BLANKS       LOCAL2           25
121000960426     C     $COM21        CAT       INDPRV:2      LOCAL2
121100960426     C                   ENDIF
121200950201     C*
121300950201     C                   ELSE
121400950201     C* SE NAZIONE ESTERA STAMPO  CAP + LOCALITA' SU UNA RIGA; STAMPO
121500960429     C*   NAZIONE + DECODIFICA NAZIONE  NELLA RIGA SOTTO.
121600960429     C*   SE ESTERO NON STAMPO LA PROVINCIA
121700960429     C  N33              MOVEL     INDCIT        LOCAL3           21
121800950201     C                   ENDIF
121900000000      *
122000000000      * VALORIZZO DATI FILIALE E DATA SOLO PER FV1 E FV6 = 'S'
122100000000     C     *IN21         IFEQ      '0'
122200000000     C     *IN26         OREQ      '0'
122300000000     C     *IN27         OREQ      '0'
122400120611     c                   eval      wpo = $filia
122500120611     c                   exsr      ctrpo
122600120611     c                   eval      $filia = wpo
122700000000     C     $FILIA        CHAIN     AZORG01L                           35
122800000000      *
122900000000     C     *IN27         IFEQ      '0'
123000000000     C  N35              MOVEL     ORGDES        TD7FIL           20
123100000000     C   35              MOVEL     *BLANKS       TD7FIL
123200000000     C                   ENDIF
123300000000      *
123400000000     C     *IN21         IFEQ      '0'
123500000000     C  N35              MOVEL     ORGDES        TD1FIL           20
123600000000     C   35              MOVEL     *BLANKS       TD1FIL
123700000000      *
123800000000     C                   MOVEL     ORGIND        TD1VIA           25
123900000000     C                   MOVEL     ORGCPF        TD1CAP            5 0
124000940315     C                   MOVE      *BLANKS       TD1LOC           24
124100940315     C     ORGLOC        CAT       ORGPRO:2      TD1LOC
124200000000     C                   MOVEL     ORGTEL        TD2TEL           13
124300000000     C                   MOVEL     ORGFAX        TD2TFX           13
124400000000     C                   ENDIF
124500000000      *
124600960910     C* COSTRUISCO LOCALITA' + DATA
124700060609     C                   CLEAR                   LOCDAT           35
124800060609     C                   MOVEL     I45GDS        WDATA
124900060609      *
125000060609     c                   if        $codlin = 'I'
125100060609     c                   eval      locdat = orgdes + ', li '+
125200060609     c                             wgg + '/' + wmm + '/' + waa
125300060609     c                   else
125400060609     c                   eval      locdat = orgdes + ', ' +
125500060609     c                             wgg + '/' + wmm + '/' + waa
125600060609     c                   endif
125700000000     C                   ENDIF
125800000000      *
125900000000      * CONTROLLO ESISTENZA RIFERIMENTI, SOLO SE RICHIESTI
126000021022     C     §TXSRI        IFEQ      'S'
126100981113     C     I45NR1        COMP      *BLANKS                            61
126200981113     C  N61I45NR2        COMP      *BLANKS                            61
126300000000     C                   ELSE
126400000000     C                   SETOFF                                       61
126500000000     C                   ENDIF
126600060608
126700060608      *? CHIODO ATTENZIONE!!!!! ?
126800060608      * purtroppo devo chiodare la stampa delle costanti in lingua
126900060608      * il controllo lo faccio con il campo chiave che ho usato per
127000060608      * reperire il testo
127100060608      * ITALIANO
127200060608     c                   if        $codlin = 'I'
127300070319     c                   eval      let01 = 'Filiale di:'
127400060608     c                   eval      let02 = 'Spett.le'
127500060608     c                   eval      let03 = 'Tel. '
127600060608     c                   eval      let04 = 'Fax  '
127700060608     c                   eval      let05 = 'ns. rif.'
127800110209     c                   eval      let05 = %trim(let05) + ' ' +
127900060608     c                             i45nr1 + ' / ' + i45nr2
128000060608     c                   evalr     let06 = 'C.A.'
128100060608     c  n98              eval      let07 = 'Telefonando chiedere del Sig.'
128200060608     c  n98              eval      let07 = %trim(let07) + ' ' + descom
128300060608     c   98              eval      let07 = 'Telefonando chiedere dell''Uf-
128400060608     c                             ficio Commerciale'
128500070319     c                   eval      let08 = 'Filiale di'
128600060612     c                   eval      let08 = %trim(let08) + ' ' + td7fil
128700060608     c                   endif
128800060608      * INGLESE
128900060608     c                   if        $codlin = '2'
129000070319     c                   eval      let01 = 'Branch:'
129100060608     c                   eval      let02 = 'Messrs.'
129200060608     c                   eval      let03 = 'Tel. '
129300060608     c                   eval      let04 = 'Fax  '
129400060608     c                   eval      let05 = 'our ref.'
129500110209     c                   eval      let05 = %trim(let05) + ' ' +
129600060608     c                             i45nr1 + ' / ' + i45nr2
129700060608     c                   evalr     let06 = 'For the attention of'
129800060608     c  n98              eval      let07 = 'When phoning, please ask for Mr.'
129900060608     c  n98              eval      let07 = %trim(let07) + ' ' + descom
130000060608     c   98              eval      let07 = 'Telefonando chiedere dell''Uf-
130100060608     c                             ficio Commerciale'
130200070319     c                   eval      let08 = 'Branch'
130300060612     c                   eval      let08 = %trim(let08) + ' ' + td7fil
130400060608     c                   endif
130500060608      * FRANCESE
130600060608     c                   if        $codlin = '3'
130700070319     c                   eval      let01 = 'Filiale de:'
130800060608     c                   eval      let02 = 'Société'
130900060608     c                   eval      let03 = 'Tél. '
131000060608     c                   eval      let04 = 'Fax  '
131100060608     c                   eval      let05 = 'notre réf.'
131200110209     c                   eval      let05 = %trim(let05) + ' ' +
131300060608     c                             i45nr1 + ' / ' + i45nr2
131400060608     c                   evalr     let06 = 'A l''attention de'
131500060608     c  n98              eval      let07 = 'Au téléphone, demander M.'
131600060608     c  n98              eval      let07 = %trim(let07) + ' ' + descom
131700060608     c   98              eval      let07 = 'Telefonando chiedere dell''Uf-
131800060608     c                             ficio Commerciale'
131900070319     c                   eval      let08 = 'Filiale de'
132000060612     c                   eval      let08 = %trim(let08) + ' ' + td7fil
132100060608     c                   endif
132200060608      * TEDESCO
132300060608     c                   if        $codlin = '4'
132400070319     c                   eval      let01 = 'Filiale in:'
132500060608     c                   eval      let02 = 'An'
132600060608     c                   eval      let03 = 'Tel. '
132700060608     c                   eval      let04 = 'Fax  '
132800060608     c                   eval      let05 = 'unser zeichen'
132900110209     c                   eval      let05 = %trim(let05) + ' ' +
133000060608     c                             i45nr1 + ' / ' + i45nr2
133100060608     c                   evalr     let06 = 'Zu Hd.'
133200060608     c  n98              eval      let07 = 'Telefonischer Ansprechpartner: Herr'
133300060608     c  n98              eval      let07 = %trim(let07) + ' ' + descom
133400060608     c   98              eval      let07 = 'Telefonando chiedere dell''Uf-
133500060608     c                             ficio Commerciale'
133600070319     c                   eval      let08 = 'Filiale in'
133700060612     c                   eval      let08 = %trim(let08) + ' ' + td7fil
133800060608     c                   endif
133900000000      *
134000000000      * STAMPA TESTATA
134100041029     c                   select
134200041029     c     *in08         wheneq    *off
134300041029     c     *in09         andeq     *off
134400041029     c                   except    testa
134500041029     c     *in09         wheneq    *on
134600041029     c   33              except    testafm
134700041029     c  n33              except    testanm
134800041029     c     *in08         wheneq    *on
134900041029     c   33              except    testaf
135000041029     c  n33              except    testan
135100041029     c                   endsl
135200000000      *
135300000000     C                   ENDSR
135400000000      *************************************************************************
135500000000      * ROUTINE DI STAMPA CORPO LETTERA
135600000000      *************************************************************************
135700000000     C     CORLET        BEGSR
135800000000      *
135900021025     c                   eval      *in01 = *off
136000021025     c                   eval      *in02 = *off
136100021025
136200021022     C     KTxt          SETLL     FnTxt01l
136300000000      *
136400000000DO  1C                   DO        *HIVAL
136500021022     C     KTxt          READE     FnTxt01l                               31
136600021022      *
136700021022      * Salto il record se annullato o se tipo H
136800021022IF  2C  N31              IF        TxtAtb <>'A' and TxtPos <>'H'
136900000000      *
137000000000      * MEMORIZZO SE CI SONO RIGHE IN CALCE
137100000000IF  3C     TXTPOS        IFEQ      'F'
137200000000     C     TXTNRR        IFEQ      1
137300000000     C                   MOVEL     TXTRIG        CAL1            110
137400000000     C                   MOVEL     TXTRIB        CALRB1            1
137500021025     C                   eval      *in01 = *on
137600000000     C                   ELSE
137700000000     C                   MOVEL     TXTRIG        CAL2            110
137800000000     C                   MOVEL     TXTRIB        CALRB2            1
137900021025     C                   eval      *in02 = *on
138000000000     C                   END
138100000000      *
138200000000X   3C                   ELSE
138300000000      *
138400000000      * SE NON HO LETTO RIGHE DI CALCE INIZIO A STAMPARE
138500000000      * STAMPO RIGA + SALTO PAGINA
138600961129    4C     TXTPOS        IFEQ      'S'
138700021025      *
138800021025    5c                   if        *in08 = *off
138900041029     c                             and *in09 = *off
139000021025     C                   EXCEPT    RIG
139100021025     c                   else
139200041029     c                   if        *in09 = *on
139300041029     C   33              EXCEPT    RIGFM
139400041029     C  N33              EXCEPT    RIGNM
139500041029     c                   else
139600021025     C   33              EXCEPT    RIGF
139700021025     C  N33              EXCEPT    RIGN
139800021025    5c                   endif
139900041029    5c                   endif
140000021025      *
140100000000      * SE RIBATTITURA RISTAMPA
140200961129    5C     TXTRIB        IFEQ      'R'
140300021025    6c                   if        *in08 = *off
140400041029     c                             and *in09 = *off
140500021025     C                   EXCEPT    RIG2
140600021025     c                   else
140700041029     c                   if        *in09 = *on
140800041029     C   33              EXCEPT    RIG2FM
140900041029     C  N33              EXCEPT    RIG2NM
141000041029     c                   else
141100021025     C   33              EXCEPT    RIG2F
141200021025     C  N33              EXCEPT    RIG2N
141300021025    6C                   endif
141400041029    6C                   endif
141500021025    5C                   ENDIF
141600021025      *
141700021025    5c                   IF        *in08 = *off
141800041029     c                             and *in09 = *off
141900021025     C                   eval        *inOF = *ON
142000021025     c                   ELSE
142100041029     c                   if        *in09 = *on
142200041029     C   33              eval      *inOC = *on
142300041029     C  N33              eval      *inOD = *on
142400041029     c                   else
142500021025     C   33              eval      *inOG = *on
142600021025     C  N33              eval      *inOE = *on
142700021025    5C                   ENDIF
142800041029    5C                   ENDIF
142900021025    4C                   ENDIF
143000000000      *
143100961129      * PER "OF"/"OG" STAMPO 1 RIGA IN CALCE SE C'E'
143200970326    4C     *IN08         IFEQ      *OFF
143300041029     c     *in09         andeq     *off
143400961129     C     *INOF         ANDEQ     *ON
143500970326     C*
143600970326     C     *IN08         OREQ      *ON
143700961129     C     *INOG         ANDEQ     *ON
143800970326     C     *IN33         ANDEQ     *ON
143900970326     C*
144000970326     C     *IN08         OREQ      *ON
144100970326     C     *INOE         ANDEQ     *ON
144200970326     C     *IN33         ANDEQ     *OFF
144300970326     C* STAMPO RIGHE IN CALCE
144400970326     C                   EXSR      CALCE
144500000000      *
144600000000      * VADO A SALTO PAGINA (NUMEARNDO, SE RICHIESTO)
144700970318     C   08
144800970318     CORN28              DO
144900041103     C  N09              ADD       1             $PAGIN
145000970326     C   33
145100970326     CAN 08              EXCEPT    PAG2
145200970326     C  N33
145300970326     CAN 08              EXCEPT    PAG2N
145400000000     C                   ENDDO
145500041029     c  N09
145600041029     CanN08              EXCEPT    PAG3
145700970326     C   33
145800970326     CAN 08              EXCEPT    PAG3F
145900970326     C  N33
146000970326     CAN 08              EXCEPT    PAG3N
146100041029     C  n09
146200041029     CANN08              SETOFF                                       OF
146300041029     C   33
146400041029     CAN 09              SETOFF                                       OC
146500041029     C  N33
146600041029     CAN 09              SETOFF                                       OD
146700970326     C   33
146800970326     CAN 08              SETOFF                                       OG
146900970326     C  N33
147000970326     CAN 08              SETOFF                                       OE
147100961203    4C                   ENDIF
147200000000      *
147300000000      * STAMPO RIGA SE RIGA DI LETTERA NORMALE
147400000000     C     TXTPOS        IFEQ      'C'
147500041029     c  N09
147600041029     CANN08              EXCEPT    RIG
147700041029     C   33
147800041029     CAN 09              EXCEPT    RIGFM
147900041029     C  N33
148000041029     CAN 09              EXCEPT    RIGNM
148100970326     C   33
148200970326     CAN 08              EXCEPT    RIGF
148300970326     C  N33
148400970326     CAN 08              EXCEPT    RIGN
148500000000      * SE RIBATTITURA RISTAMPA
148600000000     C     TXTRIB        IFEQ      'R'
148700041029     c  N09
148800041029     CANN08              EXCEPT    RIG2
148900041029     C   33
149000041029     CAN 09              EXCEPT    RIG2FM
149100041029     C  N33
149200041029     CAN 09              EXCEPT    RIG2NM
149300970326     C   33
149400970326     CAN 08              EXCEPT    RIG2F
149500970326     C  N33
149600970326     CAN 08              EXCEPT    RIG2N
149700000000     C                   ENDIF
149800000000     C                   ENDIF
149900000000      *
150000000000E   3C                   ENDIF
150100000000      *
150200000000E   2C                   ENDIF
150300000000E   1C  N31              ENDDO
150400000000      *
150500000000     C                   ENDSR
150600970326      *************************************************************************
150700970326      * STAMPO RIGHE IN CALCE
150800970326      *************************************************************************
150900970326     C     CALCE         BEGSR
151000970326     C     *IN02         IFEQ      *ON
151100041029     c  N09
151200041029     CANN08              EXCEPT    STCA2
151300041029     C   33
151400041029     CAN 09              EXCEPT    STCA2FM
151500041029     C  N33
151600041029     CAN 09              EXCEPT    STCA2NM
151700970326     C   33
151800970326     CAN 08              EXCEPT    STCA2F
151900970326     C  N33
152000970326     CAN 08              EXCEPT    STCA2N
152100970326     C     CALRB2        IFEQ      'R'
152200041029     c  N09
152300041029     CANN08              EXCEPT    STCAR2
152400041029     C   33
152500041029     CAN 09              EXCEPT    STCR2FM
152600041029     C  N33
152700041029     CAN 09              EXCEPT    STCR2NM
152800970326     C   33
152900970326     CAN 08              EXCEPT    STCR2F
153000970326     C  N33
153100970326     CAN 08              EXCEPT    STCR2N
153200970326     C                   ENDIF
153300970326     C                   ENDIF
153400970326     C*
153500970326      * STAMPO SECONDA RIGA IN CALCE SE C'E'
153600970326     C     *IN01         IFEQ      *ON
153700041029     c  N09
153800041029     CANN08              EXCEPT    STCA1
153900041126     C   33
154000041126     CAN 09
154100041126     CAN 02              EXCEPT    VUO1FM
154200041126     C   33
154300041126     CAN 09
154400041126     CANN02              EXCEPT    VUO2FM
154500041029     C   33
154600041029     CAN 09              EXCEPT    STCA1FM
154700041126     C  N33
154800041126     CAN 09
154900041126     CAN 02              EXCEPT    VUO1NM
155000041126     C  N33
155100041126     CAN 09
155200041126     CANN02              EXCEPT    VUO2NM
155300041029     C  N33
155400041029     CAN 09              EXCEPT    STCA1NM
155500970326     C   33
155600970326     CAN 08              EXCEPT    STCA1F
155700970326     C  N33
155800970326     CAN 08              EXCEPT    STCA1N
155900970326     C     CALRB1        IFEQ      'R'
156000041029     c  N09
156100041029     CANN08              EXCEPT    STCAR1
156200041029     C   33
156300041029     CAN 09              EXCEPT    STCR1FM
156400041029     C  N33
156500041029     CAN 09              EXCEPT    STCR1NM
156600970326     C   33
156700970326     CAN 08              EXCEPT    STCR1F
156800970326     C  N33
156900970326     CAN 08              EXCEPT    STCR1N
157000970326     C                   ENDIF
157100970326     C                   ENDIF
157200970326     C                   ENDSR
157300000000      *************************************************************************
157400000000      * PARTE FINALE DELLA LETTERA
157500000000      *************************************************************************
157600000000     C     FINLET        BEGSR
157700000000      *
157800000000      * STAMPO FIRMA IN FONDO ALLA LETTERA
157900041029     C  N09
158000041029     CANN08              EXCEPT    FIRMA
158100041029     C   33
158200041029     CAN 09              EXCEPT    FIRMAFM
158300041029     C  N33
158400041029     CAN 09              EXCEPT    FIRMANM
158500970326     C   33
158600970326     CAN 08              EXCEPT    FIRMAF
158700970326     C  N33
158800970326     CAN 08              EXCEPT    FIRMAN
158900970326     C* STAMPO RIGHE IN CALCE
159000970326     C                   EXSR      CALCE
159100940330     C*
159200940330     C                   MOVE      *BLANKS       LOCALP
159300000000      *
159400000000     C                   ENDSR
159500000000      *************************************************************************
159600000000      * ROUTINE DI DECODIFICA DATI ANAGRAFICI
159700000000      *************************************************************************
159800000000     C     DECANA        BEGSR
159900000000      *
160000000000     C                   SETOFF                                       607374
160100940331     C                   SETOFF                                       616263
160200970225     C                   SETOFF                                       1698
160300000000     C                   MOVEL     *BLANKS       DESRS1           44
160400050224     C                   MOVEL     *BLANKS       DESRS2           44
160500000000     C                   MOVE      *BLANKS       DESCOM           20
160600000000      *
160700021022      * COMMERCIALE (PER §TXSTC='S')
160800940330    1C     *IN25         IFEQ      *OFF
160900981113    2C     I45AST        IFNE      'L'
161000940330     C*
161100940330    3C     CLPAGE        IFNE      0
161200130731     C     CLPage        CHAIN     AZCMM000
161300130731     c                   if        %found(AZCMM01L)
161400130731     C                   MOVEL     CMMdes        DESCOM
161500130731     C                   endif
161600000000     C                   SETON                                        60
161700940330    3C                   ENDIF
161800970225     C* SE TESTO SEDE LB STAMPO ALLA CA UFFICIO COMMERICALE
161900970225     C     SIMFEL        IFEQ      0
162000981113     C     I45TTX        ANDEQ     'LB'
162100970225     C                   SETON                                        98
162200970225     C                   ENDIF
162300940330     C*
162400940330   X2C                   ELSE
162500940330     C*
162600940330     C* SE E' STATO PASSATO LO SCRIVO
162700981113    3C     I45DCM        IFNE      *BLANKS
162800940330     C                   SETON                                        60
162900981113     C                   MOVEL     I45DCM        DESCOM
163000940330    3C                   ENDIF
163100940330    2C                   ENDIF
163200940330    1C                   ENDIF
163300940531      *-------------------------------------------------------------------------
163400021022      * ALLA CORTESE ATTENZIONE (PER §TXSCA='S')
163500940531      * RESPONSABILE TRASPORTI
163600000000IF  1C     *IN24         IFEQ      '0'
163700940330     C*
163800981113IF  2C     I45AST        IFEQ      'C'
163900091022     C     I45AST        OREQ      'T'
164000940601     C                   SETOFF                                       36
164100940531      *
164200981113IF  3C     I45AST        IFEQ      'C'
164300940601     C                   MOVE      ACOKSC        KEY1C
164400940601     C                   MOVE      *BLANKS       NOTNK2
164500010131     C                   SETOFF                                       35
164600010131     C     TFNT2K        SETLL     TFNTC01L
164700010131     C                   DO        *HIVAL
164800010131     C     TFNT2K        READE     TFNTC01L                               38
164900010131     C   38              LEAVE
165000010131     C     NTCFLT        IFNE      'A'
165100010131     C     TFNT2K        SETLL     TFNTC01L                               35
165200010131     C                   LEAVE
165300010131     C                   ENDIF
165400010131     C                   ENDDO
165500940601E   3C                   ENDIF
165600940601      *
165700110204IF  3C     I45AST        ifeq      'T'
165800940601      * PER PRIMA COSA CERCO LE NOTE PER VISITA
165900940601     C                   MOVE      VISNRV        NOTNK1
166000940601     C                   MOVE      *BLANKS       NOTNK2
166100010131     C                   SETOFF                                       35
166200010131     C     TFNTCK        SETLL     TFNTC01L
166300010131     C                   DO        *HIVAL
166400010131     C     TFNTCK        READE     TFNTC01L                               38
166500010131     C   38              LEAVE
166600010131     C     NTCFLT        IFNE      'A'
166700010131     C     TFNTCK        SETLL     TFNTC01L                               35
166800010131     C                   LEAVE
166900010131     C                   ENDIF
167000010131     C                   ENDDO
167100940601     C   35              SETON                                        36
167200940601     C* SE NON TROVATA ED E' VISITA DI CLIENTE CODIFICATO CERCO
167300940601     C*  CON CODICE CLIENTE
167400940601IF  4C  N35VISKSC        IFNE      0
167500940601     C                   MOVE      VISKSC        KEY1C
167600940531     C                   MOVE      *BLANKS       NOTNK2
167700010131     C                   SETOFF                                       35
167800010131     C     TFNT2K        SETLL     TFNTC01L
167900010131     C                   DO        *HIVAL
168000010131     C     TFNT2K        READE     TFNTC01L                               38
168100010131     C   38              LEAVE
168200010131     C     NTCFLT        IFNE      'A'
168300010131     C     TFNT2K        SETLL     TFNTC01L                               35
168400010131     C                   LEAVE
168500010131     C                   ENDIF
168600010131     C                   ENDDO
168700940601E   4C                   ENDIF
168800940601E   3C                   ENDIF
168900030212      *
169000000000      *
169100010131      * SOLO SE E' ANDATO A BUON FINE
169200010131---  C     *IN35         IFEQ      *ON
169300010131     C                   DO        *HIVAL
169400010131     C  N36TFNT2K        READE     TFNTC01L                               35
169500010131     C   36TFNTCK        READE     TFNTC01L                               35
169600010131     C   35              LEAVE
169700010131    4C     NTCSNS        IFEQ      'S'
169800010131     C     NTCFLT        ANDNE     'A'
169900010131    5C     *IN73         IFEQ      '0'
170000010131     C                   SETON                                        7363
170100010131     C                   MOVEL     NTCRNT        DESRS1
170200010131   X5C                   ELSE
170300010131     C                   SETON                                        74
170400050224     C                   MOVEL     NTCRNT        DESRS2
170500010131    5C                   ENDIF
170600010131    4C                   ENDIF
170700010131     C                   ENDDO
170800010131---  C                   ENDIF
170900000000E   2C                   ENDIF
171000940330     C*
171100981113    2C     I45AST        IFEQ      'L'
171200981113     C     I45ACA        ANDNE     *BLANK
171300981113     C                   MOVEL     I45ACA        DESRS1
171400940331     C                   SETON                                        7363
171500940330    2C                   ENDIF
171600000000      *
171700000000E   1C                   ENDIF
171800940331     C* CODICE
171900940331     C  N23STACOD        IFNE      *BLANKS
172000940331     C                   SETON                                        6263
172100940331     C                   ENDIF
172200940331     C*
172300950201     C* DECODIFICO LA NAZIONE SOLO SE DIVERSA DA ITALIA
172400950201     C     WNAR          IFNE      *BLANKS
172500950201     C                   SETON                                        16
172600010626     C                   MOVE      *BLANKS       TBLKEY
172700950201     C                   MOVEL     '15'          TBLCOD
172800950201     C                   MOVEL(P)  WNAR          TBLKEY
172900950201     C     TABELK        CHAIN     TABEL                              34
173000950201     C  N34              MOVEL     TBLUNI        DECNAR           25
173100950201     C   34              CLEAR                   DECNAR
173200950201     C                   ENDIF
173300000000      *
173400000000     C                   ENDSR
173500050908      *************************************************************************
173600050908      * ROUTINE DI stampa spool per testo html
173700050908      *************************************************************************
173800050908     c     prthtml       begsr
173900050908     c                   clear                   pnomn           198
174000050929     c                   clear                   pCosUffCom      198
174100050929     c                   clear                   pNomPo          198
174200050929     c                   clear                   pindpo          198
174300050929     c                   clear                   pCapLocPrvPo    198
174400050929     c                   clear                   pTelPo          198
174500050929     c                   clear                   pFaxPo          198
174600050929     c                   clear                   peman           198
174700050929     c                   clear                   pnomc           198
174800120611
174900120611      * se la filiale è in £6 devo prendere la capofila
175000120611     c                   eval      wpo = keypo
175100120611     c                   exsr      ctrpo
175200120611     c                   eval      keypo = wpo
175300050929
175400050929     c* --------------------------------------
175500050929     c* chaino azorg per reperimento dati p.o.
175600050929     c* --------------------------------------
175700050929     c     keypo         chain     azorg01l
175800051012     C                   MOVEL     ORGDE3        OG143
175900050929     c* ------------------------------------------------------
176000130802     c* Chaino file AZNTC per reperimento e-mail commerciale
176100050929     c* e nominativo commerciale
176200050929     c* ------------------------------------------------------
176300050929     c                   clear                   wemn             60
176400050929     c* se ce l'ho uso il cod.comm
176500050930     c                   if        i45ast <> 'L'
176600060313     c                   if        w§cm1mitt <> *blanks
176700060313     c                   eval      wemn = %trim(w§cm1mitt) +
176800120220     c                             '@BRT.it'
176900050929     c                   else
177000050929     c* se non trovato nemmeno con keypo assumo INFOxxx o INTxxx
177100051012    2c                   if        §ogntw<>'EEX' and §ogntw<>'EUP' and
177200051012     c                             §ogntw<>'DPD' and §ogntw<>'FED'
177300051012     c                   move      orgfil        w003a
177400050930     c                   eval      wemn = 'INFO' + w003a +
177500120220     c                             '@BRT.it'
177600050929     c                   else
177700051012     c* determino ter-part del p.o.
177800051012     c                   clear                   fnlv54ds
177900051012     c                   movel     'P'           i54tpt
178000051012     c                   movel     keypo         i54lin
178100051012     c                   z-add     dateu         i54drf
178200051012     c                   call      'FNLV54R'
178300051012     C                   PARM                    fnlv54ds
178400051012     c                   move      o54tfp        w003a             3
178500050930     c                   eval      wemn = 'INT' + w003a  +
178600120220     c                             '@BRT.it'
178700050929     c                   endif
178800050929     c                   endif
178900050930     c                   else
179000050930     c                   eval      wemn = i45emn
179100050930     c                   end
179200050929     c* --------------------------------------------------------------
179300050929     c* preparo campi per scrittura file di spool per invio testo html
179400050929     c* --------------------------------------------------------------
179500050929     c* commerciale Bartolini
179600050930     c                   if        i45ast <> 'L'
179700051007     c                   eval      pnomn = '§§Firma1§§' + '#' +
179800130802     c                             %trim(wNominCmm) + '}{'
179900050930     c                   else
180000051007     c                   eval      pnomn = '§§Firma1§§' + '#' +
180100051109     c                             %trim(i45nmn) + '}{'
180200050930     c                   endif
180300050929     c* "Ufficio Commerciale"
180400051007     c                   eval      pCosUffCom = '§§Firma2§§' + '#' +
180500051109     c                             'Ufficio Commerciale' + '}{'
180600050929     c* nome po
180700051007     c                   eval      pnompo = '§§Firma3§§' + '#' +
180800070319     c                             'Filiale di:' +
180900051109     c                             %trim(orgdes) + '}{'
181000050929     c* indirizzo p.o.
181100051007     c                   eval      pindpo = '§§Firma4§§' + '#' +
181200051109     c                             %trim(orgind) + '}{'
181300050929     c* cap + loc + provincia p.o.
181400050930     c                   move      orgcpf        w005a             5
181500051007     c                   eval      pCapLocPrvPo = '§§Firma5§§' + '#' +
181600050930     c                             %trim(w005a) +' ' + %trim(orgloc) +' ' +
181700051109     c                             orgpro + '}{'
181800050929     c* telefono p.o.
181900051007     c                   eval      ptelpo = '§§Firma6§§' + '#' +
182000051109     c                             'TEL:' + %trim(orgtel) + '}{'
182100050929     c* FAX      p.o.
182200051007     c                   eval      pfaxpo = '§§Firma7§§' + '#' +
182300051109     c                             'FAX:' + %trim(orgfax) + '}{'
182400050929     c* E-mail commerciale
182500120220     c                   eval      peman = '§§nome@BRT.it§§' + '#' +
182600051109     c                             %trim(wemn) + '}{'
182700050929     c* Nominativo Cliente
182800051007     c                   eval      pnomc = '§§Nominativo§§' + '#' +
182900051109     c                             %trim(w45nmc) + '}{'
183000050929     c*
183100050908     c                   except    rigahtml
183200050908     c                   endsr
183300050908      *************************************************************************
183400050908      * ROUTINE DI stampa errori invio testo html
183500050908      *************************************************************************
183600050908     c     s_errhtml     begsr
183700050909     c* la prima volta che entro eseguo ovrprtf + apertura file di spool
183800050908     c                   if        opnerr = *blanks
183900051003     c                   movel(P)  cmerr(1)      comman
184000050908     c                   z-add     80            lung
184100050908     c                   call      'QCMDEXC'
184200050908     c                   parm                    comman
184300050908     c                   parm                    lung
184400050908     c                   open      fnlv45p
184500050908     c                   seton                                        96
184600050908     c                   movel     '1'           opnerr            1
184700050909     c* imposto campi di stampa testata
184800050909     c                   select
184900050909     c                   when      i45ast='C'
185000050909     c                   eval      p45amb = 'Clienti           '
185100091022     c                   when      i45ast='T'
185200091022     c                   eval      p45amb = 'Trattative        '
185300050909     c                   endsl
185400050909     c                   if        i45ttx <> *blanks
185500050909     c                   seton                                        29
185600050909     c                   clear                   ds1h
185700050909     C                   Z-ADD     1             TBLKUT
185800050909     C                   MOVE      '1H'          TBLCOD
185900050909     C                   MOVEL(p)  i45ttx        TBLKEY
186000050909     C     TABELK        CHAIN     TABEL00F                           99
186100050909     C  N99              MOVEL     TBLUNI        DS1h
186200050909     c                   movel     §1hdes        desttx
186300050909     c                   endif
186400050909     c                   endif
186500050909     c*
186600050909     c   96              write     er_test
186700050908     c                   setoff                                       96
186800050909     c                   select
186900050909     c                   when      i45ast='C'
187000050909     c                   movel(p)  acoksc        p45cod
187100050909     c                   movel(p)  acorag        p45rag
187200091022     c                   when      i45ast='T'
187300110204     c                   movel(p)  visnrv        p45cod
187400091022     c                   movel(p)  acorag        p45rag
187500050909     c                   endsl
187600050909     c                   write     er_riga
187700050908     c                   endsr
187800060313     c* reperimento indirizzo e-mail del commerciale ------------------
187900060313     c     RepEmaMit     begsr
188000060313     c                   clear                   w§cm1mitt        30
188100130802     c                   clear                   wNominCmm        30
188200130802if  1c                   if        i45ast <> 'L'
188300130802     c* uso il cod.comm
188400130802if  2c                   if        clpagex > *zeros
188500130802     c                   eval      k_NTCcmm = clpagex
188600130802     c                   eval      k_NTCtnt = '02'
188700130802     c     k02azntc01    chain     AZNTC000
188800130802if  3c                   if        %found(AZNTC01L)
188900130802     c                   eval      w§cm1mitt = %trim(AZNTCrnt)
189000130802e   3c                   endif
189100130802      * - reperisco anche il nominativo del comm.le
189200130802     c                   eval      k_NTCcmm = clpagex
189300130802     c                   eval      k_NTCtnt = '01'
189400130802     c     k02azntc01    chain     AZNTC000
189500130802if  3c                   if        %found(AZNTC01L)
189600130802     c                   eval      wNominCmm = %trim(AZNTCrnt)
189700130802e   3c                   endif
189800130802e   2c                   endif
189900130802e   1c                   endif
190000060313     c                   endsr
190100120611
190200120611      ****************************************************************
190300120611      * Controllo la filiale
190400120611      ****************************************************************
190500120611     c     Ctrpo         BEGSR
190600120611
190700120611     c                   clear                   fnlv55ds
190800120611     c                   eval      d55lin = wpo
190900120611     c                   eval      d55tpt = '6'
191000120611     c                   eval      d55drf = dateu
191100120611     c                   call      'FNLV55R'
191200120611     c                   parm                    fnlv55ds
191300120611     c                   IF        d55tfa <> 0
191400120611     c                   eval      wpo = d55tfa
191500120611     c                   ENDIF
191600120611
191700120611     c                   ENDSR
191800120611
191900000000      *************************************************************************
192000000000      * ROUTINE DI DEFINIZIONI KLIST E VARIABILI
192100000000      *************************************************************************
192200000000     C     DEFVAR        BEGSR
192300000000      *
192400000000     C     TABELK        KLIST
192500000000     C                   KFLD                    TBLKUT
192600000000     C                   KFLD                    TBLCOD
192700000000     C                   KFLD                    TBLKEY
192800050930     C     KTBE          KLIST
192900050930     C                   KFLD                    TBECOD
193000050930     C                   KFLD                    TBEKE1
193100050930     C                   KFLD                    TBEKE2
193200130802       // -?File AZNTC01L: Note/Rubrica Commerciali?
193300130802     c     k02azntc01    klist
193400130802     c                   kfld                    k_NTCcmm
193500130802     c                   kfld                    k_NTCtnt
193600000000     C     CNACOK        KLIST
193700000000     C                   KFLD                    ACOKUT
193800000000     C                   KFLD                    ACOKCC
193900000000     C                   KFLD                    ACOKSC
194000021022     C     KTxt          KLIST
194100021022     C                   KFLD                    $Testo
194200060608     c                   kfld                    $codpo
194300060608     c                   kfld                    $codlin
194400021022     C     KTxt2         KLIST
194500021022     C                   KFLD                    $Testo
194600060608     c                   kfld                    $codpo
194700060608     c                   kfld                    $codlin
194800021022     C                   KFLD                    $TpRig            1
194900000000     C     TFNTCK        KLIST
195000000000     C                   KFLD                    APPL              1
195100000000     C                   KFLD                    NOTNK1           11
195200000000     C                   KFLD                    NOTNK2            4
195300000000     C                   KFLD                    NOTTNT            2
195400000000     C     TFNT2K        KLIST
195500000000     C                   KFLD                    APPLC             1
195600000000     C                   KFLD                    KEY1C            11
195700000000     C                   KFLD                    NOTNK2            4
195800000000     C                   KFLD                    NOTTNT            2
195900041029     C     Kntc          KLIST
196000041029     C                   KFLD                    kapl
196100041029     C                   KFLD                    knk1
196200041029     C                   KFLD                    knk2
196300041029     C                   KFLD                    ktnt
196400950201     C*
196500950201     C* DEFINIZIONE CAMPI
196600950201     C     *LIKE         DEFINE    INDCAE        WCAP
196700130802     c     *like         define    AZNTCcmm      k_NTCcmm
196800130802     c     *like         define    AZNTCtnt      k_NTCtnt
196900000000     C                   ENDSR
197000000000      *************************************************************************
197100000000      *
197200961128     OLET198    E   N21      TESTA            10
197300060608     O                  33   LET01               34
197400060608     O                 N33   LET01               31
197500961119     O          E   N21      TESTA            11
197600960422     O                  33   TD1FIL              29
197700960422     O                 N33   TD1FIL              26
197800961119      * ----  Spett.le  ( 33ON = compresso, 33OFF = normale )
197900961119     O          E   N22      TESTA            12
198000060608     O                  33   LET02               76
198100070301     O                 N33   LET02               54
198200961119     O          E   N21      TESTA            12
198300960422     O                  33   TD1VIA              34
198400960422     O                 N33   TD1VIA              31
198500961119      * ----  Ragione sociale (1° parte se non compresso)
198600961119     O          E   N22      TESTA            13
198700961119     O                  33   WRAG               112
198800961119     O                 N33   RAGIO1              75
198900961119     O          E   N21      TESTA            13
199000960422     O                  33   TD1CAP              14
199100960422     O                  33   TD1LOC              39
199200960422     O                 N33   TD1CAP              11
199300960422     O                 N33   TD1LOC              36
199400961119      * ----  Ragione sociale (2° parte)
199500961119     O          E   N22N33   TESTA            14
199600961119     O                       RAGIO2              61
199700961119     O          E   N21      TESTA            14
199800060608     O                  33   LET03               14
199900060608     O                 N33   LET03               11
200000960422     O                  33   TD2TEL              28
200100960422     O                 N33   TD2TEL              25
200200961119      * ----  indirizzo destinatario
200300961119     O          E   N22      TESTA            15
200400961119     O                  33   INDVIA              96
200500961119     O                 N33   INDVIA              74
200600961119     O          E   N21      TESTA            15
200700060608     O                  33   LET04               14
200800060608     O                 N33   LET04               11
200900960422     O                  33   TD2TFX              28
201000960422     O                 N33   TD2TFX              25
201100960426      * ----  localita' destinatario
201200960426     O          E   N22      TESTA            16
201300960429     O* ESTERO (16ON )  TESTO COMPRESSO (33ON )
201400960429     O                  33 16WCAP                75
201500960429     O                  33 16INDCIT             106
201600960429     O* ESTERO (16ON )  TESTO NORMALE   (33OFF)
201700960426     O                 N33 16WCAP                53
201800960426     O                 N33 16LOCAL3              75
201900960429     O* ITALIA (16OFF)  TESTO NORMALE   (33OFF)
202000960426     O                 N33N16WCAP5               49
202100960426     O                 N33N16LOCAL2              75
202200960429     O* ITALIA (16OFF)  TESTO COMPRESSO (33ON )
202300960429     O                  33N16WCAP                75
202400970306     O                  33N16LOCALP             106
202500060608      * ----  stampa dei riferimenti (61 ON)
202600940331     O          E    61      TESTA            17
202700060608     O                  33   LET05               34
202800060608     O                 N33   LET05               31
202900950201     O* ----  nazione
203000960426     O          E   N22 16   TESTA            17
203100960429     O                  33   WNAR                69
203200960429     O                  33   DECNAR              96
203300960429     O                 N33   WNAR                47
203400960429     O                 N33   DECNAR              74
203500940331     O* ----  codice
203600940331     O          E   N23 62   TESTA       2
203700980803     O                  33                       16 'Cod.Cl.'
203800980803     O                  33   STACOD              28
203900980803     O                 N33                       13 'Cod.Cl.'
204000980803     O                 N33   STACOD              25
204100940314      * ----  Alla c.a.
204200940331     O          E       73   TESTA            21
204300060608     O                  33   LET06               64
204400960426     O                  33   DESRS1             110
204500060608     O                 N33   LET06               42
204600960426     O                 N33   DESRS1              88
204700940314      * ----  e p.c.
204800940331     O          E       74   TESTA       1
204900050224     O                  33   DESRS2             110
205000050224     O                 N33   DESRS2              88
205100940314      * ----  Telefonando chiedere del sig.
205200940331     O          E    63 60   TESTA       2
205300940331     O         OR   N63 60                    21
205400060608     O                  33   LET07               64
205500060608     O                 N33   LET07               61
205600940314      * ----  localita' e data
205700940331     O          E    63N26   TESTA       2
205800940331     O         OR   N63N26 60            2
205900940331     O         OR   N63N26N60                 21
206000960910     O                  33   LOCDAT              44
206100960910     O                 N33   LOCDAT              41
206200940331     O          E            TESTA       2
206300940331     O                                              ' '
206400980227      *-------------------------------------------------------------------------
206500980227      * SE NON VIENE SCELTO NESSUNO DATOP IN TESTATA FACCIO UN SALTO DI 6
206600980227      * RIGHE
206700980227     O          E    21 22 23TESTA       3  3
206800980227     O         AND   24 25 26
206900980227     O         AND   27 28
207000980227     O                                              ' '
207100000000      *-------------------------------------------------------------------------
207200961119     O          E            PAG3             10
207300000000     O                                           63 ' '
207400000000      *-------------------------------------------------------------------------
207500000000     O          E            RIG         1
207600000000     O                       TXTRIG             113
207700000000     O          E            RIG2        0  0
207800000000     O                       TXTRIG             113
207900000000      *-------------------------------------------------------------------------
208000980401     O          E            STCA2            62
208100000000     O                       CAL2               113
208200000000     O          E            STCAR2      0  0
208300000000     O                       CAL2               113
208400000000      *-------------------------------------------------------------------------
208500980401     O          E            STCA1            63
208600000000     O                       CAL1               113
208700000000     O          E            STCAR1      0  0
208800000000     O                       CAL1               113
208900000000      *-------------------------------------------------------------------------
209000971001     O          E   N27      FIRMA       3
209100020626COMP O                  33   RSUTold             90
209200020626NORM O                 N33   RSUTold             64
209300971001     O          E   N27      FIRMA       1
209400060612COMP O                  33   LET08              116
209500060612NORM O                 N33   LET08               90
209600961129     O*
209700961129     O**************************************************************************
209800961129     O**** I N V I O    C O N    F A X *****************************************
209900961129     O**************************************************************************
210000000920     OLET017    E            TESTAF           09
210100970326     O                       STAIMM              51
210200970326     O                                           55 'FAX:'
210300961203     O                       INDTLF              72
210400970326     O                                          106 'Pagina'
210500970326     O                       $PAGIN        Z    110
210600961203     O          E   N21      TESTAF           10
210700060608     O                       LET01               34
210800961129     O          E   N21      TESTAF           11
210900970326     O                       TD1FIL              29
211000961129      * ----  Spett.le  ( 33ON = compresso, 33OFF = normale )
211100961129     O          E   N22      TESTAF           12
211200060608     O                       LET02               76
211300961129     O          E   N21      TESTAF           12
211400970326     O                       TD1VIA              34
211500970326      * ----  Ragione sociale
211600961129     O          E   N22      TESTAF           13
211700970326     O                       WRAG               112
211800961129     O          E   N21      TESTAF           13
211900970326     O                       TD1CAP              14
212000970326     O                       TD1LOC              39
212100961129     O          E   N21      TESTAF           14
212200060608     O                       LET03               14
212300970326     O                       TD2TEL              28
212400961129      * ----  indirizzo destinatario
212500961129     O          E   N22      TESTAF           15
212600970326     O                       INDVIA              96
212700961129     O          E   N21      TESTAF           15
212800060608     O                       LET04               14
212900970326     O                       TD2TFX              28
213000961129      * ----  localita' destinatario
213100961129     O          E   N22      TESTAF           16
213200970326     O* ESTERO (16ON )
213300970326     O                     16WCAP                75
213400970326     O                     16INDCIT             106
213500970326     O* ITALIA (16OFF)
213600970326     O                    N16WCAP                75
213700970326     O                    N16LOCALP             110
213800961129      * ----  stampa dei riferimenti (61 ON)
213900961129     O          E    61      TESTAF           17
214000060608     O                       LET05               34
214100961129     O* ----  nazione
214200961129     O          E   N22 16   TESTAF           17
214300970326     O                       WNAR                69
214400970326     O                       DECNAR              96
214500961129     O* ----  codice
214600961129     O          E   N23 62   TESTAF      2
214700980803     O                                           16 'Cod.'
214800980803     O                       STACOD              28
214900961129      * ----  Alla c.a.
215000961129     O          E       73   TESTAF           21
215100060608     O                       LET06               64
215200970326     O                       DESRS1             110
215300961129      * ----  e p.c.
215400961129     O          E       74   TESTAF      1
215500050224     O                       DESRS2             110
215600961129      * ----  Telefonando chiedere del sig.
215700961129     O          E    63 60   TESTAF      2
215800961129     O         OR   N63 60                    21
215900060608     O                       LET07               64
216000961129      * ----  localita' e data
216100961129     O          E    63N26   TESTAF      2
216200961129     O         OR   N63N26 60            2
216300961129     O         OR   N63N26N60                 21
216400970326     O                       LOCDAT              44
216500961129     O          E            TESTAF      2
216600961129     O                                              ' '
216700961129      *-------------------------------------------------------------------------
216800970318     O          E            PAG2             09
216900970326     O                                          106 'Pagina'
217000970326     O                       $PAGIN        Z    110
217100961129     O          E            PAG3F            10
217200961129     O                                           63 ' '
217300961129      *-------------------------------------------------------------------------
217400961129     O          E            RIGF        1
217500961129     O                       TXTRIG             113
217600961129     O          E            RIG2F       0  0
217700961129     O                       TXTRIG             113
217800961129      *-------------------------------------------------------------------------
217900961129     O          E            STCA2F           64
218000961129     O                       CAL2               113
218100961129     O          E            STCR2F      0  0
218200961129     O                       CAL2               113
218300961129      *-------------------------------------------------------------------------
218400961129     O          E            STCA1F           65
218500961129     O                       CAL1               113
218600961129     O          E            STCR1F      0  0
218700961129     O                       CAL1               113
218800961129      *-------------------------------------------------------------------------
218900971001     O          E   N27      FIRMAF      3
219000020626COMP O                       RSUTold             90
219100971001     O          E   N27      FIRMAF      1
219200060612COMP O                       LET08              116
219300961129     O*
219400970326     O**************************************************************************
219500970326     OLETNOR    E            TESTAN           09
219600970326     O                       STAIMM              51
219700970326     O                                           55 'FAX:'
219800970326     O                       INDTLF              72
219900970326     O                                           79 'Pagina'
220000970326     O                       $PAGIN        Z     83
220100970326     O          E   N21      TESTAN           10
220200060608     O                       LET01               31
220300970326     O          E   N21      TESTAN           11
220400970326     O                       TD1FIL              26
220500970326     O          E   N22      TESTAN           12
220600060608     O                       LET02               54
220700970326     O          E   N21      TESTAN           12
220800970326     O                       TD1VIA              31
220900970326      * ----  Ragione sociale (1° PARTE)
221000970326     O          E   N22      TESTAN           13
221100970326     O                       RAGIO1              75
221200970326     O          E   N21      TESTAN           13
221300970326     O                       TD1CAP              11
221400970326     O                       TD1LOC              36
221500970326      * ----  Ragione sociale (2° parte)
221600970326     O          E   N22      TESTAN           14
221700970326     O                       RAGIO2              61
221800970326     O          E   N21      TESTAN           14
221900060608     O                       LET03               11
222000970326     O                       TD2TEL              25
222100970326      * ----  indirizzo destinatario
222200970326     O          E   N22      TESTAN           15
222300970326     O                       INDVIA              74
222400970326     O          E   N21      TESTAN           15
222500060608     O                       LET04               11
222600970326     O                       TD2TFX              25
222700970326      * ----  localita' destinatario
222800970326     O          E   N22      TESTAN           16
222900970326     O* ESTERO (16ON )  TESTO NORMALE
223000970326     O                     16WCAP                53
223100970326     O                     16LOCAL3              75
223200970326     O* ITALIA (16OFF)  TESTO NORMALE
223300970326     O                    N16WCAP5               49
223400970326     O                    N16LOCAL2              75
223500970326      * ----  stampa dei riferimenti (61 ON)
223600970326     O          E    61      TESTAN           17
223700060608     O                       LET05               31
223800970326     O* ----  nazione
223900970326     O          E   N22 16   TESTAN           17
224000970326     O                       WNAR                47
224100970326     O                       DECNAR              74
224200970326     O* ----  codice
224300970326     O          E   N23 62   TESTAN      2
224400970326     O                                           10 'Cod.'
224500970326     O                       STACOD              22
224600970326      * ----  Alla c.a.
224700970326     O          E       73   TESTAN           21
224800060608     O                       LET06               42
224900970326     O                       DESRS1              88
225000970326      * ----  e p.c.
225100970326     O          E       74   TESTAN      1
225200050224     O                       DESRS2              88
225300970326      * ----  Telefonando chiedere del sig.
225400970326     O          E    63 60   TESTAN      2
225500970326     O         OR   N63 60                    21
225600060608     O                       LET07               61
225700970326      * ----  localita' e data
225800970326     O          E    63N26   TESTAN      2
225900970326     O         OR   N63N26 60            2
226000970326     O         OR   N63N26N60                 21
226100970326     O                       LOCDAT              41
226200970326     O          E            TESTAN      2
226300970326     O                                              ' '
226400970326      *-------------------------------------------------------------------------
226500970326     O          E            PAG2N            09
226600970326     O                                           79 'Pagina'
226700970326     O                       $PAGIN        Z     83
226800970326     O          E            PAG3N            10
226900970326     O                                           63 ' '
227000970326      *-------------------------------------------------------------------------
227100970326     O          E            RIGN        1
227200970326     O                       TXTRIG             113
227300970326     O          E            RIG2N       0  0
227400970326     O                       TXTRIG             113
227500970326      *-------------------------------------------------------------------------
227600970326     O          E            STCA2N           64
227700970326     O                       CAL2               113
227800970326     O          E            STCR2N      0  0
227900970326     O                       CAL2               113
228000970326      *-------------------------------------------------------------------------
228100970326     O          E            STCA1N           65
228200970326     O                       CAL1               113
228300970326     O          E            STCR1N      0  0
228400970326     O                       CAL1               113
228500970326      *-------------------------------------------------------------------------
228600971001     O          E   N27      FIRMAN      3
228700020626NORM O                       RSUTold             64
228800971001     O          E   N27      FIRMAN      1
228900060612NORM O                       LET08               90
229000041029     O**************************************************************************
229100041029     O**** I N V I O    C O N    E-MAIL ****************************************
229200041029     O**************************************************************************
229300041104     Olet017m   E   N21      TESTAFM          02
229400060608     O                       LET01               34
229500041104     O          E   N21      TESTAFM          03
229600041029     O                       TD1FIL              29
229700041029      * ----  Spett.le  ( 33ON = compresso, 33OFF = normale )
229800041104     O          E   N22      TESTAFM          04
229900060608     O                       LET02               76
230000041104     O          E   N21      TESTAFM          04
230100041029     O                       TD1VIA              34
230200041029      * ----  Ragione sociale
230300041104     O          E   N22      TESTAFM          05
230400041029     O                       WRAG               112
230500041104     O          E   N21      TESTAFM          05
230600041029     O                       TD1CAP              14
230700041029     O                       TD1LOC              39
230800041104     O          E   N21      TESTAFM          06
230900060608     O                       LET03               14
231000041029     O                       TD2TEL              28
231100041029      * ----  indirizzo destinatario
231200041104     O          E   N22      TESTAFM          07
231300041029     O                       INDVIA              96
231400041104     O          E   N21      TESTAFM          07
231500060608     O                       LET04               14
231600041029     O                       TD2TFX              28
231700041029      * ----  localita' destinatario
231800041104     O          E   N22      TESTAFM          08
231900041029     O* ESTERO (16ON )
232000041029     O                     16WCAP                75
232100041029     O                     16INDCIT             106
232200041029     O* ITALIA (16OFF)
232300041029     O                    N16WCAP                75
232400041029     O                    N16LOCALP             110
232500041029      * ----  stampa dei riferimenti (61 ON)
232600041104     O          E    61      TESTAFM          09
232700060608     O                       LET05               34
232800041029     O* ----  nazione
232900041104     O          E   N22 16   TESTAFM          09
233000041029     O                       WNAR                69
233100041029     O                       DECNAR              96
233200041029     O* ----  codice
233300041029     O          E   N23 62   TESTAFM     2
233400041029     O                                           16 'Cod.'
233500041029     O                       STACOD              28
233600041029      * ----  Alla c.a.
233700041104     O          E       73   TESTAFM          13
233800060608     O                       LET06               64
233900041029     O                       DESRS1             110
234000041029      * ----  e p.c.
234100041029     O          E       74   TESTAFM     1
234200050224     O                       DESRS2             110
234300041029      * ----  Telefonando chiedere del sig.
234400041029     O          E    63 60   TESTAFM     2
234500041104     O         OR   N63 60                    13
234600060608     O                       LET07               64
234700041029      * ----  localita' e data
234800041029     O          E    63N26   TESTAFM     2
234900041029     O         OR   N63N26 60            2
235000041104     O         OR   N63N26N60                 13
235100041029     O                       LOCDAT              44
235200041029     O          E            TESTAFM     2
235300041029     O                                              ' '
235400041029      *-------------------------------------------------------------------------
235500041029     O          E            RIGFM       1
235600041029     O                       TXTRIG             113
235700041029     O          E            RIG2FM      0  0
235800041029     O                       TXTRIG             113
235900041029      *-------------------------------------------------------------------------
236000041126     O          E            STCA2FM     2  0
236100041029     O                       CAL2               113
236200041029     O          E            STCR2FM     0  0
236300041029     O                       CAL2               113
236400041126      *-------------------------------------------------------------------------
236500041126     O          E            VUO1FM      1  0
236600041126      *-------------------------------------------------------------------------
236700041126     O          E            VUO2FM      2  0
236800041029      *-------------------------------------------------------------------------
236900041126     O          E            STCA1FM     0  0
237000041029     O                       CAL1               113
237100041029     O          E            STCR1FM     0  0
237200041029     O                       CAL1               113
237300041029      *-------------------------------------------------------------------------
237400041029     O          E   N27      FIRMAFM     3
237500041029COMP O                       RSUTold             90
237600041029     O          E   N27      FIRMAFM     1
237700060612COMP O                       LET08              116
237800041029     O*
237900041029     O**************************************************************************
238000041104     Oletnorm   E   N21      TESTANM          02
238100060608     O                       LET01               31
238200041104     O          E   N21      TESTANM          03
238300041029     O                       TD1FIL              26
238400041104     O          E   N22      TESTANM          04
238500060608     O                       LET02               54
238600041104     O          E   N21      TESTANM          04
238700041029     O                       TD1VIA              31
238800041029      * ----  Ragione sociale (1° PARTE)
238900041104     O          E   N22      TESTANM          05
239000041029     O                       RAGIO1              75
239100041104     O          E   N21      TESTANM          05
239200041029     O                       TD1CAP              11
239300041029     O                       TD1LOC              36
239400041029      * ----  Ragione sociale (2° parte)
239500041104     O          E   N22      TESTANM          06
239600041029     O                       RAGIO2              61
239700041104     O          E   N21      TESTANM          06
239800060608     O                       LET03               11
239900041029     O                       TD2TEL              25
240000041029      * ----  indirizzo destinatario
240100041104     O          E   N22      TESTANM          07
240200041029     O                       INDVIA              74
240300041104     O          E   N21      TESTANM          07
240400060608     O                       LET04               11
240500041029     O                       TD2TFX              25
240600041029      * ----  localita' destinatario
240700041104     O          E   N22      TESTANM          08
240800041029     O* ESTERO (16ON )  TESTO NORMALE
240900041029     O                     16WCAP                53
241000041029     O                     16LOCAL3              75
241100041029     O* ITALIA (16OFF)  TESTO NORMALE
241200041029     O                    N16WCAP5               49
241300041029     O                    N16LOCAL2              75
241400041029      * ----  stampa dei riferimenti (61 ON)
241500041104     O          E    61      TESTANM          09
241600060608     O                       LET05               31
241700041029     O* ----  nazione
241800041104     O          E   N22 16   TESTANM          09
241900041029     O                       WNAR                47
242000041029     O                       DECNAR              74
242100041029     O* ----  codice
242200041029     O          E   N23 62   TESTANM     2
242300041029     O                                           10 'Cod.'
242400041029     O                       STACOD              22
242500041029      * ----  Alla c.a.
242600041104     O          E       73   TESTANM          13
242700060608     O                       LET06               42
242800041029     O                       DESRS1              88
242900041029      * ----  e p.c.
243000041029     O          E       74   TESTANM     1
243100050224     O                       DESRS2              88
243200041029      * ----  Telefonando chiedere del sig.
243300041029     O          E    63 60   TESTANM     2
243400041104     O         OR   N63 60                    13
243500060608     O                       LET07               61
243600041029      * ----  localita' e data
243700041029     O          E    63N26   TESTANM     2
243800041029     O         OR   N63N26 60            2
243900041104     O         OR   N63N26N60                 13
244000041029     O                       LOCDAT              41
244100041029     O          E            TESTANM     2
244200041029     O                                              ' '
244300041029      *-------------------------------------------------------------------------
244400041029     O          E            RIGNM       1
244500041029     O                       TXTRIG             113
244600041029     O          E            RIG2NM      0  0
244700041029     O                       TXTRIG             113
244800041029      *-------------------------------------------------------------------------
244900041126     O          E            STCA2NM     2  0
245000041029     O                       CAL2               113
245100041029     O          E            STCR2NM     0  0
245200041029     O                       CAL2               113
245300041126      *-------------------------------------------------------------------------
245400041126     O          E            VUO1NM      1  0
245500041126      *-------------------------------------------------------------------------
245600041126     O          E            VUO2NM      2  0
245700041029      *-------------------------------------------------------------------------
245800041126     O          E            STCA1NM     0  0
245900041029     O                       CAL1               113
246000041029     O          E            STCR1NM     0  0
246100041029     O                       CAL1               113
246200041029      *-------------------------------------------------------------------------
246300041029     O          E   N27      FIRMANM     3
246400041029NORM O                       RSUTold             64
246500041029     O          E   N27      FIRMANm     1
246600060612NORM O                       LET08               90
246700050908     O**************************************************************************
246800050908     O**** I N V I O    C O N    E-MAIL XTML ***********************************
246900050908     O**************************************************************************
247000051007     Olethtml   E            rigahtml       1
247100051007     o                       pnomc
247200051007     O          E            rigahtml       1
247300050929     o                       pnomn
247400050908     O          E            rigahtml       1
247500050929     o                       pCosUffCom
247600050908     O          E            rigahtml       1
247700050929     o                       pNomPo
247800050929     O          E            rigahtml       1
247900050929     o                       pIndPo
248000050929     O          E            rigahtml       1
248100050929     o                       pCapLocPrvPo
248200050929     O          E            rigahtml       1
248300050929     o                       pTelPo
248400050929     O          E            rigahtml       1
248500050929     o                       pFaxPo
248600050929     O          E            rigahtml       1
248700050929     o                       peman
248800000000      *************************************************************************
248900000000** Comandi    567890123456789012345678901234567890123456789012345678901234567890
249000000920OVRPRTF FILE(LET198) CPI(15)   COPIES(001)  USRDTA('Lettere') FORMTYPE(LET1)
249100961129**
249200000920OVRPRTF LET017  LET198 CPI(15)   OUTQ(          ) FORMTYPE(LET1)      1
249300000920  COPIES(001) USRDTA('LetFax')                                        2
249400041102**
249500041102DLTOVR FILE(       )
249600041102**
249700041102OVRPRTF LET017M LET198 CPI(15)   OUTQ(          ) FORMTYPE(LET1)      1
249800041102  COPIES(001) USRDTA('LetMail')  usrdfndta('                          2
249900050908**
250000050908OVRPRTF LETHTML LET198 CPI(15)   OUTQ(          ) FORMTYPE(LET1)      1
250100050908  COPIES(001) USRDTA('LetHtml')  usrdfndta('                          2
250200050908**
250300050908OVRPRTF FNLV45P USRDTA('ErrHtml')                                     1
