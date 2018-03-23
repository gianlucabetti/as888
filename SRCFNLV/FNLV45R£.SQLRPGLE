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
002200021025     FLET198    O    F  132        PRINTER  USROPN  OFLIND(*INOF)
002300021025     FLET017    O    F  124        PRINTER  USROPN  OFLIND(*INOG)
002400021025     FLETNOR    O    F  132        PRINTER  USROPN  OFLIND(*INOE)
002500041029     FLET017M   O    F  124        PRINTER  USROPN  OFLIND(*INOC)
002600041029     FLETNORM   O    F  132        PRINTER  USROPN  OFLIND(*INOD)
002700050908     FLEThtml   O    F  198        PRINTER  USROPN
002800050908     Ffnlv45p   O    e             PRINTER  USROPN  oflind(*IN96)
002900000000      *
003000020927     d PrmFine         S              1
003100020927     d PrmFil          S              3S 0
003200020927     d PrmTpOutQ       S              1
003300020927     d PrmOutQ         S             10
003400020927     d PrmOutQLib      S             10
003500020927     d PrmEsito        S              1
003600041029     d kapl            s                   like(ntcapl)
003700041029     d knk1            s                   like(ntcnk1)
003800041029     d knk2            s                   like(ntcnk2) inz('    ')
003900050908     d ktnt            s                   like(ntctnt)
004000050908     d w45nmc          s                   like(i45nmc)
004100050912     d w45htm          s                   like(i45htm)
004200051007     d clpagex         s                   like(clpage)
004300060608     d $codlin         s                   like(i45lin)
004400060608     d $codpo          s                   like(simfel)
004500060919     d wgiro           S              1
004600060919     d w_noelab        S              1
004700060608
004800060608     d let01           s             25
004900060608     d let02           s             10
005000060608     d let03           s              5
005100060608     d let04           s              5
005200060608     d let05           s             25
005300060608     d let06           s             20
005400060608     d let07           s             55
005500060612     d let08           s             46
005600020927
005700000000      * SCHIERA STAMPA INTESTAZIONE LETTERA
005800000000     D CMD             S              1    DIM(80) CTDATA PERRCD(80)
005900960910     D SDT             S              1    DIM(15) CTDATA PERRCD(15)
006000961203     D CM2             S             70    DIM(2) CTDATA PERRCD(1)
006100961203     D SKI             S              1    DIM(140)
006200041102     D SKIM            S              1    DIM(140)
006300050908     D SKIh            S              1    DIM(140)
006400041102     D CMO             S              1    DIM(30) CTDATA PERRCD(30)
006500041102     D CMM             S             70    DIM(2) CTDATA PERRCD(1)
006600050908     D CMMh            S             70    DIM(2) CTDATA PERRCD(1)
006700050909     D CMerr           S             33    DIM(1)  CTDATA PERRCD(1)
006800020626
006900020626     d AzuteDs       e ds                  ExtName(AzUte00F)
007000020626     d DDatiUte      e ds
007100020626     d Tibs34Ds      e ds                  Inz
007200021108      *
007300021108     d Trul31ds      e ds                  INZ
007400091002     d  pog                   10    759  0 dim(250)
007500021108      *
007600020626
007700970225     D KPJBA         E DS
007800060919     d P_numcli              498    502
007900021025     D DFNTXTH       E DS
008000050909     D DS1h          E DS
008100050930     D Dhtm          E DS
008200060919     D Dvpo          E DS
008300051012     D og143         E DS
008400021025     D FNLV45DS      E DS
008500120611     d fnlv55ds      e ds
008600021025     D TIBS55DS      E DS
008700091002     D TRUL42DS      E DS
008800041029     D TRUL44DS      E DS
008900050902     D TRTCM1DS      E DS
009000050929     D fnlv54ds      E DS
009100060919     D tibs02ds      E DS
009200021025
009300940315     D                 DS
009400091002     D  ragsoc                 1     48
009500960426     D  RAGIO1                 1     31
009600960426     D  RAGIO2                32     48
009700021025      *
009800981113     D WLBDA8          DS
009900981113     D  G08DAT                 1      8  0
010000981113     D  G08INV                 9     16  0
010100981113     D  G08ERR                17     17
010200981113     D  G08TGI                18     22  0
010300960910      *
010400960910     D                 DS
010500960910     D  WGG                    1      2
010600960910     D  WMM                    3      4
010700960910     D  WAA                    5      8
010800960910     D  WDATA                  1      8
010900091001
011000110909     d StringaSql      s           2048    Varying
011100091002     d xa              s              5i 0 inz(*zeros)
011200091002     d cnacods       e ds                  extname(cnaco00f)
011300091002     d cnindds       e ds                  extname(cnind00f)
011400091002     d cnclpds       e ds                  extname(cnclp00f)
011500091127     d tivisds       e ds                  extname(tivis00f)
011600091002     d $visaco         s               n
011700120611     d wpo             s              3  0
011800021025
011900021025      *----------------------------------------------------------------
012000021025
012100000000     C     *ENTRY        PLIST
012200000000     C                   PARM                    KPJBA
012300021025     C                   PARM                    fnlv45ds
012400020626
012500020626      * reperisco i dati utente
012600020626     c     *dtaara       define    §azute        AzuteDs
012700020626     c     *dtaara       define    §datiute      DDatiUte
012800020626
012900020626     c                   in(E)     *dtaara
013000020626     c                   IF        %Error or RSUT = *blanks
013100020626     c                   call      'TIBS34R'
013200020626     c                   parm                    Tibs34Ds
013300020626     c                   in        *dtaara
013400020626     c                   ENDIF
013500091002
013600091002     c/exec sql
013700091002     c+     set  option  DynUsrPrf = *Owner,
013800091002     c+                  CloSqlCsr = *EndMod
013900091002     c/end-exec
014000020626
014100040804      * SE NON SONO IN SEDE
014200970319     C     SIMFEL        IFGT      *ZEROS
014300021108      * Chiamo il trul31r per reperire schiera po
014400021108      * stesso raggruppamento po utente /stesso AS
014500021108     C                   CLEAR                   TRUL31DS
014600041214     C                   EVAL      I31ABI   = 'RA'
014700021108     C                   EVAL      I31CPO   = DutPou
014800021108     C                   CALL      'TRUL31R'
014900021108     C                   PARM                    KPJBA
015000021108     C                   PARM                    TRUL31DS
015100970319     C                   END
015200021025      *
015300981113     C                   TIME                    W0140            14 0
015400050912     C                   MOVEL     W0140         UTIME             6 0
015500981113     C                   MOVE      W0140         WDTGIO            8 0
015600981113     C                   Z-ADD     WDTGIO        G08DAT
015700981113     C                   MOVEL     *BLANK        G08ERR
015800981113     C                   CALL      'XSRDA8'
015900981113     C                   PARM                    WLBDA8
016000981113     C                   MOVEL     G08INV        DATEU             8 0
016100060919     c*
016200060919     c* reperisco variabili programmi operativi
016300060919     C                   CLEAR                   tibs02ds
016400060919     c                   clear                   dvpo
016500060919     C                   MOVEL     'C'           T02MOD
016600060919     C                   MOVEL     KNSIF         T02SIF
016700060919     C                   MOVEL     'VPO'         T02COD
016800060919     c                   movel(P)  'VPO'         t02ke1
016900060919     C                   CALL      'TIBS02R'
017000060919     C                   PARM                    KPJBA
017100060919     C                   PARM                    tibs02ds
017200060919    2C                   IF        T02ERR = *BLANKS
017300060919     C                   MOVEL     T02uni        dvpo
017400060919    2C                   ENDIF
017500021025
017600000000      *************************************************************************
017700021025
017800961203     C                   MOVEA     CM2(1)        SKI(1)
017900961203     C                   MOVEA     CM2(2)        SKI(71)
018000041102     C                   MOVEA     CMM(1)        SKIM(1)
018100041102     C                   MOVEA     CMM(2)        SKIM(71)
018200050908     C                   MOVEA     CMMh(1)       SKIh(1)
018300050908     C                   MOVEA     CMMh(2)       SKIh(71)
018400981113     C                   MOVEL     I45NCO        W003A             3
018500000920     C                   MOVEA     W003A         CMD(39)
018600000920     C                   MOVEA     W003A         SKI(80)
018700041102     C                   MOVEA     W003A         SKIM(80)
018800110204     C                   MOVE      I45TTX        $TESTO            2
018900021022      *
019000021022      * Aggancio per il tipo testo il file testi record 'H'
019100021022    1C     I45TTX        IFNE      *BLANKS
019200050912     c     i45htm        andeq     *blanks
019300021022     C                   MOVE      'H'           $TpRig
019400060608     c                   eval      $codlin = i45lin
019500060608     c                   eval      $codpo = simfel
019600021022     C     KTxt2         chain     FnTxt02l                           03
019700060608      * se non trovo il testo ed è stata richiesta la stampa in lingua
019800060608      * provo a cercare il testo con il p.o. di sede (000)
019900060608     c                   if        not %found(fntxt02l) and i45lin <> 'I'
020000060608     c                   eval      $codpo = 0
020100060608     c     ktxt2         chain     fntxt02l                           03
020200060608      * se non trovo il testo neanche con il p.o. di sede recupero il
020300060613      * testo in italiano con il p.o.
020400060608     c                   if        not %found(fntxt02l) and i45lin <> 'I'
020500060613     c                   eval      $codpo = simfel
020600060608     c                   eval      $codlin = 'I'
020700060608     c     ktxt2         chain     fntxt02l                           03
020800060613      * se non trovo il testo in italiano riprovo con il p.o. di sede
020900060612     c                   if        not %found(fntxt02l) and i45lin <> 'I'
021000060613     c                   eval      $codpo = 0
021100060612     c     ktxt2         chain     fntxt02l                           03
021200060612     c                   endif
021300060608     c                   endif
021400060608     c                   endif
021500021025     c  n03              movel     TxtRig        DfntxtH
021600021025     c   03              clear                   DfntxtH
021700000000      *
021800000000      * CONTROLLO SUL VALORE DEI FLAG PER LA STAMPA DEI DATI ANAGRAFICI
021900021022     C     §TXSIP        COMP      'S'                                2121
022000021022     C     §TXSIC        COMP      'S'                                2222
022100021022     C     §TXSCC        COMP      'S'                                2323
022200021022     C     §TXSCA        COMP      'S'                                2424
022300021022     C     §TXSTC        COMP      'S'                                2525
022400021022     C     §TXSDC        COMP      'S'                                2626
022500021022     C     §TXSPF        COMP      'S'                                2727
022600021022     C     §TXSNP        COMP      'S'                                2828
022700000000      *
022800000000      * Prima di aprire il file di stampa controllo se compresso o no
022900021022    2C     §TXFTS        IFEQ      'C'
023000000920     C                   MOVEA     '(16,7)'      CMD(25)
023100000920     C                   MOVEA     '(16,7)'      SKI(27)
023200000920     C                   MOVEA     'LET017'      SKI(9)
023300041122     C                   MOVEA     '(16,7)'      SKIM(27)
023400041102     C                   MOVEA     'LET017M'     SKIM(9)
023500021025     C                   eval      *in33 = *on
023600000000     C                   ELSE
023700021025     C                   MOVEA     '(10)  '      CMD(25)
023800021025     C                   MOVEA     '(10)  '      SKI(27)
023900970326     C                   MOVEA     'LETNOR'      SKI(9)
024000041102     C                   MOVEA     '(10)  '      SKIM(27)
024100041102     C                   MOVEA     'LETNORM'     SKIM(9)
024200021025     C                   eval      *in33 = *off
024300940330    2C                   ENDIF
024400000000      *
024500000000      * Eseguo OVRPRTF sul file di stampa
024600000000     C                   Z-ADD     80            LUNG             15 5
024700000000     C                   MOVEL     *BLANKS       COMMAN           80
024800000000     C                   MOVEA     CMD           COMMAN
024900000000     C                   CALL      'QCMDEXC'                            H1
025000000000     C                   PARM                    COMMAN
025100000000     C                   PARM                    LUNG
025200000000     C                   OPEN      LET198
025300961129     C*
025400041029     C* ESEGUO OVRPRTF SUL FILE DI STAMPA PER INVIO TRAMITE FAX
025500981113     C     I45FAX        IFEQ      'S'
025600981113     C     I45AST        ANDNE     'L'
025700050908     C                   EXSR      OVRFAX
025800981113     C     I45IMM        IFEQ      'I'
025900970326     C                   MOVEL     '*'           STAIMM            1
026000970326    3C                   ENDIF
026100970326    2C                   ENDIF
026200970326    1C                   ENDIF
026300021025
026400940330      * Ho 4 strade tra cui scegliere:
026500091127      * + 1 per le trattative
026600981113      *   1)  I45AST='C'  utilizzare i clienti effettivi
026700981113      *   2)  I45AST='V'  utilizzare le visite
026800981113      *   3)  I45AST='P'  utilizzare i clienti potenziali
026900981113      *   3)  I45AST='L'  libero
027000091127      *   5)  I45AST='T'  utilizzare le trattative
027100021025
027200021025     c                   SELECT
027300021025     C                   when      I45AST  = 'L'
027400940330     C                   EXSR      STRAD4
027500021025     C                   when      I45AST  = 'C'
027600000000     C                   EXSR      STRAD1
027700091127     C                   when      I45AST  = 'T'
027800091127     C                   EXSR      STRAD5
027900021025     c                   ENDSL
028000000000      *
028100000000     C                   CLOSE     LET198
028200000920     C   33              CLOSE     LET017
028300970326     C  N33              CLOSE     LETNOR
028400050909     c
028500050909     c                   if        opnerr = '1'
028600050909     c                   write     fines
028700050909     c                   close     fnlv45p
028800050909     c                   endif
028900021025
029000021025     C                   eval      *inLR = *on
029100050929     c*
029200050929     C                   MOVEL     'C'           I54TLA
029300050929     C                   CALL      'FNLV54R'
029400050929     C                   PARM                    fnlv54ds
029500961203      *************************************************************************
029600961203      * ROUTINE PER CREARE SPOOL PER INVIO FAX
029700961203      *************************************************************************
029800961203     C     OVRFAX        BEGSR
029900021025      *
030000021025      * ESEGUO OVRPRTF PER CREARE UNO SPOOL PER LE LETTERE DA INVIARE
030100021025      *   TRAMITE FAX
030200021025      *
030300971205     C* DETERMINO LA OUTQ
030400020927
030500020927     c                   Eval      PrmFine = 'L'
030600020927     c                   Eval      PrmFil = Dutpou
030700020927     c                   Eval      PrmTpOutq = '2'
030800020927     c                   Clear                   PrmOutQ
030900020927     c                   Clear                   PrmOutQLib
031000020927     c                   Clear                   PrmEsito
031100020927     c                   Call      'TRULOUTQ'
031200020927     c                   Parm                    PrmFine
031300020927     c                   Parm                    PrmFil
031400020927     c                   Parm                    PrmTpOutQ
031500020927     c                   Parm                    PrmOutQ
031600020927     c                   Parm                    PrmOutQLib
031700020927     c                   Parm                    PrmEsito
031800020927     c                   If        PrmEsito = '0' and PrmOutq <> *Blanks
031900020927     c                   Movea     PrmOutq       Ski(39)
032000020927     c                   Else
032100020927     c                   Movea     '*SAME     '  Ski(39)
032200020927     c                   EndIf
032300971205     C*
032400961203     C                   Z-ADD     140           LUNG
032500961203     C                   MOVEL     *BLANKS       COMM            140
032600961203     C                   MOVEA     SKI           COMM
032700961203     C                   CALL      'QCMDEXC'                            H1
032800961203     C                   PARM                    COMM
032900961203     C                   PARM                    LUNG
033000961203     C*
033100000920     C   33              OPEN      LET017
033200041028     c   33              move      '1'           opnlet            1
033300970326     C  N33              OPEN      LETNOR
033400041028     c  n33              move      'L'           opnlet
033500961203     C*
033600961203     C                   ENDSR
033700041029      *************************************************************************
033800041029      * ROUTINE PER CREARE SPOOL PER INVIO e-mail
033900041029      *************************************************************************
034000041029     C     OVRmail       BEGSR
034100041029      *
034200041029      * ESEGUO OVRPRTF PER CREARE UNO SPOOL PER LE LETTERE DA INVIARE
034300041029      *   TRAMITE E-mail
034400041029      *
034500041029     c                   select
034600041029     c     opnletm       wheneq    '7'
034700041029     c                   close     let017m
034800041102     c                   movea     'LET017M'     cmo(13)
034900041102     C                   Z-ADD     80            LUNG             15 5
035000041102     C                   MOVEL     *BLANKS       COMMAN           80
035100041102     C                   MOVEA     CMO           COMMAN
035200041102     C                   CALL      'QCMDEXC'                            H1
035300041102     C                   PARM                    COMMAN
035400041102     C                   PARM                    LUNG
035500041029     c     opnletm       wheneq    'N'
035600041029     c                   close     letnorm
035700041102     c                   movea     'LETNORM'     cmo(13)
035800041102     C                   Z-ADD     80            LUNG             15 5
035900041102     C                   MOVEL     *BLANKS       COMMAN           80
036000041102     C                   MOVEA     CMO           COMMAN
036100041102     C                   CALL      'QCMDEXC'                            H1
036200041102     C                   PARM                    COMMAN
036300041102     C                   PARM                    LUNG
036400041029     c                   endsl
036500041029     c                   clear                   opnletm
036600041029     c*
036700041102     c                   clear                   trul44ds
036800041029     c                   eval      d44pgm = 'FNLV45R'
036900041102     c                   z-add     dutpou        d44po
037000041103     c     i45ast        ifne      'L'
037100041029     c                   eval      d44apl = kapl
037200041029     c                   eval      d44nk1 = knk1
037300041029     c                   eval      d44nk2 = knk2
037400041029     c                   eval      d44tnt = ktnt
037500041103     c                   else
037600041103     c                   eval      d44des = i45mai
037700041103     c                   endif
037800050912     c                   eval      §cm1var  = '*OBJM*' + i45obj
037900050912     c                   select
038000091022     c                   when      i45ast = 'T'
038100110204     c                   move      visnrv        w007a
038200091022     c                   eval      §cm1var= %trim(§cm1var) + '-T.'     +
038300091022     c                             w007a
038400050912     c                   when      i45ast = 'C'
038500050912     c                   move      acoksc        w007a             7
038600050912     c                   eval      §cm1var= %trim(§cm1var) + '-C.'      +
038700050912     c                             w007a
038800050912     c                   endsl
038900060313     c                   exsr      RepEmaMit
039000060313     c                   eval      §cm1mitt = w§cm1mitt
039100041029     c                   call      'TRUL44R'
039200041029     c                   parm                    trul44ds
039300050902     c                   parm                    trtcm1ds
039400041102     c     d44err        ifeq      *blanks
039500041102     c* eseguo ovprtf
039600050908     c                   movea     d44outq       skim(39)
039700041102     C                   MOVEL     *BLANKS       COMMM           395
039800050908     C                   MOVEA     SKIm          COMMM
039900041102     c                   eval      commm = %trim(commm) + d44dta
040000041102     c                             + ''')'
040100041102     c                   z-add     395           lungm            15 5
040200041102     C                   CALL      'QCMDEXC'                            H1
040300041102     C                   PARM                    COMMM
040400041102     C                   PARM                    LUNGM
040500041102     c
040600041029     C*
040700041029     C   33              OPEN      LET017M
040800041029     c   33              move      '7'           opnletm           1
040900041029     C  N33              OPEN      LETNORM
041000041029     c  N33              move      'N'           opnletm
041100041102     c                   endif
041200041029     C*
041300041029     C                   ENDSR
041400050908      *************************************************************************
041500050908      * ROUTINE PER CREARE SPOOL PER INVIO testo html
041600050908      *************************************************************************
041700050908     C     OVRhtml       BEGSR
041800050908      *
041900050908      * ESEGUO OVRPRTF PER CREARE UNO SPOOL PER LE LETTERE DA INVIARE
042000050908      *   TRAMITE E-mail HTML
042100050908      *
042200050908     c* Eseguo dltovr
042300050908     c                   if        opnlethtm <> ' '
042400050908     c                   close     lethtml
042500050908     c                   movea     'LETHTML'     cmo(13)
042600050908     C                   Z-ADD     80            LUNG             15 5
042700050908     C                   MOVEL     *BLANKS       COMMAN           80
042800050908     C                   MOVEA     CMO           COMMAN
042900050908     C                   CALL      'QCMDEXC'                            H1
043000050908     C                   PARM                    COMMAN
043100050908     C                   PARM                    LUNG
043200050908     c                   clear                   opnlethtm
043300050908     c                   endif
043400050908     c*
043500050908     c                   clear                   trul44ds
043600050912     c                   eval      d44pgm = 'FNLV45RHTM'
043700050908     c                   z-add     dutpou        d44po
043800050908     c     i45emc        ifeq      *blanks
043900050908     c                   eval      d44apl = kapl
044000050908     c                   eval      d44nk1 = knk1
044100050908     c                   eval      d44nk2 = knk2
044200050908     c                   eval      d44tnt = '06'
044300050908     c                   else
044400050908     c                   eval      d44des = i45emc
044500050908     c                   endif
044600050912     c                   eval      §cm1var= '*OBJM*' + %trim(i45obj)
044700050909     c                   select
044800091022     c                   when      i45ast = 'T'
044900110204     c                   move      visnrv        w007a
045000091022     c                   eval      §cm1var= %trim(§cm1var) + '-T.'     +
045100091022     c                             w007a
045200050909     c                   when      i45ast = 'C'
045300050909     c                   move      acoksc        w007a             7
045400050912     c                   eval      §cm1var= %trim(§cm1var) + '-C.'      +
045500050912     c                             w007a
045600050909     c                   endsl
045700050912     c                   eval      §cm1var = %trim(§cm1var) + '§*EMLOFT*' +
045800050912     c                             $testo + '.OFT'
045900060313     c                   exsr      RepEmaMit
046000060313     c                   eval      §cm1mitt = w§cm1mitt
046100050912     c
046200050909     c
046300050908     c                   call      'TRUL44R'
046400050908     c                   parm                    trul44ds
046500050908     c                   parm                    trtcm1ds
046600050908     c     d44err        ifeq      *blanks
046700050908     c* eseguo ovprtf
046800050908     c                   movea     d44outq       skih(39)
046900050908     C                   MOVEL     *BLANKS       COMMM           395
047000050908     C                   MOVEA     SKIh          COMMM
047100050908     c                   eval      commm = %trim(commm) + d44dta
047200050908     c                             + ''')'
047300050908     c                   z-add     395           lungm            15 5
047400050908     C                   CALL      'QCMDEXC'                            H1
047500050908     C                   PARM                    COMMM
047600050908     C                   PARM                    LUNGM
047700050908     c
047800050908     C*
047900050908     C                   OPEN      LEThtml
048000050908     c                   move      '1'           opnlethtm         1
048100050908     c                   endif
048200050908     C*
048300050908     C                   ENDSR
048400000000      *************************************************************************
048500000000      * STRADA 1:  clienti effettivi
048600000000      *************************************************************************
048700000000     C     STRAD1        BEGSR
048800000000      *
048900091002      * preparo la stringa SQL
049000091002     c                   exsr      sr_sql1
049100091001
049200060919     c                   eval      wgiro='1'
049300060919     c                   clear                   wcontcli          9 0
049400060919     c     I_strad1      tag
049500091002
049600091002     C                   MOVEL     I45KCC        NOTNK1
049700981113     C                   MOVEL     I45KCC        KEY1C
049800000000     C                   MOVEL     '05'          NOTTNT
049900000000     C                   MOVEL     'C'           APPL
050000000000     C                   MOVEL     'C'           APPLC
050100000000     C                   MOVEL     *BLANKS       NOTNK2
050200091002
050300091002      * apro il cursore da leggere
050400091002     C/EXEC SQL
050500091002     C+ Open C1
050600091002     C/END-EXEC
050700091002
050800091002     c                   Do        *hival
050900091002
051000091002      * leggo il cursore
051100091002     C/EXEC SQL
051200091002     C+ Fetch next from C1 into :cnacods, :cnindds, :cnclpds
051300091002     C/END-EXEC
051400091002
051500091002     c                   if        sqlcod = 100
051600091002     c                   leave
051700091002     c                   endif
051800091002     c                   if        sqlcod < 0
051900091006     c                   seton                                        H1
052000091002     c                   leavesr
052100091002     c                   endif
052200000000      *
052300060919     c* se sono nel primo giro devo solo contare il numero dei clienti
052400060919     c* da elaborare quindi torno a leggere
052500060919     c                   if        wgiro='1'
052600060919     c                   add       1             wcontcli
052700091002     c                   iter
052800060919     c                   endif
052900000000      *
053000000000      * Il codice filiale e' nei primi tre caratteri del cd.cliente
053100000000     C                   MOVEL     ACOKSC        $FILIA            3 0
053200940315      *
053300091002     c                   movel     acorag        ragsoc
053400960426     C                   MOVEL     ACORAG        WRAG             46
053500940331     C                   MOVEL     ACOKSC        STACOD           11
053600950201     C                   MOVEL     INDCAE        WCAP
053700950201     C                   MOVEL     INDSTA        WNAR              3
053800000000      *
053900041029     C* CONTROLLO SE INVIO TRAMITE E-MAIL/FAX OPPURE NO
054000041029     c                   movel     'C'           kapl
054100041029     c                   movel     acokcc        knk1
054200041029     c                   move      acoksc        knk1
054300050912     c                   movel     i45htm        w45htm
054400060313     c                   movel     clpage        keypo
054500060313     c                   move      clpage        clpagex
054600961203     C                   EXSR      CTRFAX
054700961129      *
054800000000      * Stampo la lettera per il cliente
054900050912     c                   if        i45htm = *blanks
055000060919     c                   if        w_noelab = ' '
055100000000     C                   EXSR      TESLET
055200000000     C                   EXSR      CORLET
055300000000     C                   EXSR      FINLET
055400060919     c                   endif
055500050908     c                   else
055600050908     c                   if        d44err = *blanks
055700050908     c                   exsr      prthtml
055800050908     c                   else
055900050908     c                   exsr      s_errhtml
056000050908     c                   endif
056100050908     c                   endif
056200000000      *
056300000000      * Pulisco i campi anagrafici
056400000000     C                   MOVE      *BLANKS       ACORAG
056500960426     C                   MOVE      *BLANKS       WRAG
056600091002     C                   MOVE      *BLANKS       ragsoc
056700000000     C                   MOVE      *BLANKS       INDVIA
056800000000     C                   MOVE      *BLANKS       INDCIT
056900000000     C                   MOVE      *BLANKS       INDPRV
057000941230     C                   CLEAR                   WCAP
057100950201     C                   CLEAR                   WNAR
057200941230     C                   CLEAR                   INDCAE
057300000000     C                   Z-ADD     *ZEROS        CLPAGE
057400000000      *
057500000000     C                   ENDDO
057600091002
057700091002     C/EXEC SQL
057800091002     C+ CLOSE C1
057900091002     C/END-EXEC
058000060919     c*
058100060919     c                   if        wgiro='1'
058200060919     c                   if        wcontcli <=§vponclimx
058300060919     c                   eval      wgiro='2'
058400060919     c                   goto      I_strad1
058500060919     c                   else
058600060919     c* passo al chiamante il numero dei clienti elaborabili per
058700060919     c* l'emissione del msg di errore
058800060919     c                   if        wcontcli >= 99999
058900060919     c                   move      '99999'       p_numcli
059000060919     c                   else
059100060919     c                   move      wcontcli      p_numcli
059200060919     c                   endif
059300060919     c                   endif
059400060919     c                   else
059500060919     c                   clear                   p_numcli
059600060919     c                   endif
059700000000      *
059800000000     C                   ENDSR
059900940330      *************************************************************************
060000940330      * STRADA 4:  libero
060100940330      *************************************************************************
060200940330     C     STRAD4        BEGSR
060300940330      *
060400940330      *
060500050908     c* TESTO TRADIZIONALE
060600050912     c                   if        i45htm = *blanks
060700940330      * Imposto i campi anagrafici
060800981113     C                   Z-ADD     I45APP        $FILIA
060900091002     C                   MOVEL(P)  I45RAG        acorag
061000091002     C                   MOVEL(P)  I45RAG        ragsoc
061100981113     C                   MOVEL(P)  I45RAG        WRAG
061200981113     C                   MOVEL     I45VIA        INDVIA
061300981113     C                   MOVEL     I45CIT        INDCIT
061400981113     C                   MOVEL     I45PRO        INDPRV
061500981113     C                   MOVEL     I45CAP        WCAP
061600981113     C                   MOVEL     I45NAR        WNAR
061700940330     C                   CLEAR                   STACOD
061800970324     C*
061900041103     c                   select
062000041103     c     i45fax        wheneq    'S'
062100041103     c     i45mai        andne     *blanks
062200060919     c     i45fax        oreq      'M'
062300041103     c                   exsr      ovrmail
062400041103     c                   seton                                        09
062500041103     C     I45FAX        wheneq    'S'
062600970326     C                   EXSR      OVRFAX
062700970324     C                   SETON                                        08
062800981113     C                   MOVEL     I45NFX        INDTLF
062900981113     C     I45IMM        IFEQ      'I'
063000970326     C                   MOVEL     '*'           STAIMM
063100970326     C                   ENDIF
063200041103     C                   ENDsl
063300940330      *
063400940330      * Stampo la lettera per il cliente
063500940330     C                   EXSR      TESLET
063600940330     C                   EXSR      CORLET
063700940330     C                   EXSR      FINLET
063800050908     c* TESTO HTML
063900050908     c                   else
064000050912     c                   movel     i45htm        w45htm
064100050908     c                   exsr      ovrhtml
064200050908     c                   clear                   w45nmc
064300050909     c                   eval      w45nmc = i45nmc
064400050929     c                   z-add     dutpou        keypo
064500050908     c                   exsr      prthtml
064600050908     c
064700050908     c                   endif
064800940330      *
064900940330     C                   ENDSR
065000091127
065100091127      *-----------------------------------------------------------------------*
065200091127      * STRADA 5:  Trattative
065300091127      *-----------------------------------------------------------------------*
065400091127     c     Strad5        Begsr
065500091127
065600091127      * controllo se sql su visite e anagrafiche provvisorie
065700091127     c                   if        i45sct <> 0 or i45clv <> *blanks or
065800091127     c                             i45pro <> *blanks or
065900091127     c                             i45cap <> *blanks or
066000091127     c                             (i45nar <> '999' and i45nar <> '   ')
066100091127     c                   eval      $visaco = *on
066200091127     c                   else
066300091127     c                   eval      $visaco = *off
066400091127     c                   endif
066500091127
066600091127      * preparo la stringa SQL
066700091127     c                   exsr      sr_sql5
066800091127
066900091127      * Apro i file interessati
067000091127     c                   if        not $visaco
067100091127     C                   OPEN      cnaCO00F
067200091127     C                   OPEN      cnIND00F
067300091127     C                   OPEN      cnCLP00F
067400091127     C                   OPEN      TFACO00F
067500091127     C                   OPEN      TFIND00F
067600091127     C                   OPEN      TFCLP00F
067700091127     c                   endif
067800091127
067900091127     c                   eval      wgiro='1'
068000091127     c                   clear                   wcontcli          9 0
068100091127     c     I_strad5      tag
068200091127      *
068300091127     C                   MOVEL     I45KCC        NOTNK1
068400091127     C                   MOVEL     I45KCC        KEY1C
068500091127     C                   MOVEL     '05'          NOTTNT
068600091127     C                   MOVEL     i45ast        APPL
068700091127     C                   MOVEL     'C'           APPLC
068800091127     C                   MOVEL     *BLANKS       NOTNK2
068900091127
069000091127      * apro il cursore da leggere
069100091127     c                   select
069200091127     c                   when      $visaco and i45ast = 'T'
069300091127     C/EXEC SQL
069400091127     C+ Open C5a
069500091127     C/END-EXEC
069600091127     c                   when      not $visaco and i45ast = 'T'
069700091127     C/EXEC SQL
069800091127     C+ Open C5b
069900091127     C/END-EXEC
070000091127     c                   endsl
070100091127
070200091127     c                   Do        *hival
070300091127
070400091127      * leggo il cursore
070500091127     c                   select
070600091127     c                   when      $visaco and i45ast = 'T'
070700091127     C/EXEC SQL
070800091127     C+ Fetch next from C5a into :tivisds,
070900091127     C+                          :cnacods, :cnindds, :cnclpds
071000091127     C+
071100091127     C/END-EXEC
071200091127     c                   when      not $visaco and i45ast = 'T'
071300091127     C/EXEC SQL
071400091127     C+ Fetch next from C5b into :tivisds
071500091127     C/END-EXEC
071600091127     c                   endsl
071700091127
071800091127     c                   if        sqlcod = 100
071900091127     c                   leave
072000091127     c                   endif
072100091127     c                   if        sqlcod < 0
072200091127     c                   seton                                        lr
072300091127     c                   leavesr
072400091127     c                   endif
072500091127
072600091127     c* se sono nel primo giro devo solo contare il numero dei clienti
072700091127     c* da elaborare quindi torno a leggere
072800091127     c                   if        wgiro='1'
072900091127     c                   add       1             wcontcli
073000091127     c                   iter
073100091127     c                   endif
073200091127      *
073300091127      * Decodifico subito il codice cliente dall'anagrafica visite/offerte
073400091127     C                   Z-ADD     1             ACOKUT
073500091127     C                   Z-ADD     I45KCC        ACOKCC
073600110204     C                   Z-ADD     visnrv        ACOKSC
073700091127     c* preparo anche la chiave per ricerca nota06
073800091127     c                   movel     i45ast        kapl
073900091127     c                   movel     acokcc        knk1
074000091127     c                   move      acoksc        knk1
074100091127     C     CNACOK        CHAIN     TFACO000                           99
074200091127     C  N99CNACOK        CHAIN     TFIND000                           99
074300091127     C  N99CNACOK        CHAIN     TFCLP000                           99
074400091127      *
074500091127      * Se non trovato cerco in quella effettiva
074600091127     C   99              DO
074700110204     C                   Z-ADD     visksc        ACOKSC
074800091127     C     CNACOK        CHAIN     CNACO000                           99
074900091127     C  N99CNACOK        CHAIN     CNIND000                           99
075000091127     C  N99CNACOK        CHAIN     CNCLP000                           99
075100091127     C                   ENDDO
075200091127
075300091127      *
075400091127      * Rilevo il codice filiale (primi tre caratteri del codice ...)
075500110204     C     visksc        IFLE      0
075600110204     C                   MOVEL     viscmm        $FILIA
075700091127     C                   MOVEL     *BLANKS       STACOD
075800091127     C                   ELSE
075900110204     C                   MOVEL     visksc        $FILIA
076000110204     C                   MOVEL     visksc        STACOD           11
076100091127     C                   ENDIF
076200091127     C*
076300110204     C                   Z-ADD     viscmm        CLPAGE
076400091127     C                   MOVEL     ACORAG        ragsoc
076500091127     C                   MOVEL     ACORAG        WRAG
076600091127     C                   MOVEL     INDCAE        WCAP
076700091127     C                   MOVEL     INDSTA        WNAR
076800091127      *
076900091127     C* CONTROLLO SE INVIO TRAMITE E-MAIL/FAX OPPURE NO
077000091127     c                   movel     i45htm        w45htm
077100110204     c                   movel     viscmm        keypo             3 0
077200110204     c                   move      viscmm        clpagex
077300091127     C                   EXSR      CTRFAX
077400091127      *
077500091127      * Stampo la lettera per il cliente
077600091127     c                   if        i45htm = *blanks
077700091127     c                   if        w_noelab =' '
077800091127     C                   EXSR      TESLET
077900091127     C                   EXSR      CORLET
078000091127     C                   EXSR      FINLET
078100091127     c                   endif
078200091127     c                   else
078300091127     c                   if        d44err = *blanks
078400091127     c                   exsr      prthtml
078500091127     c                   else
078600091127     c                   exsr      s_errhtml
078700091127     c                   endif
078800091127     c                   endif
078900091127      *
079000091127      * Pulisco i campi anagrafici
079100091127     C                   MOVE      *BLANKS       ACORAG
079200091127     C                   MOVE      *BLANKS       WRAG
079300091127     C                   MOVE      *BLANKS       ragsoc
079400091127     C                   MOVE      *BLANKS       INDVIA
079500091127     C                   MOVE      *BLANKS       INDCIT
079600091127     C                   MOVE      *BLANKS       INDPRV
079700091127     C                   CLEAR                   INDCAE
079800091127     C                   CLEAR                   WCAP
079900091127     C                   CLEAR                   WNAR
080000091127     C                   MOVE      *ZEROS        CLPAGE
080100091127      *
080200091127     C                   ENDDO
080300091127
080400091127     c                   select
080500091127     c                   when      $visaco and i45ast = 'T'
080600091127     C/EXEC SQL
080700091127     C+ CLOSE C5a
080800091127     C/END-EXEC
080900091127     c                   when      not $visaco and i45ast = 'T'
081000091127     C/EXEC SQL
081100091127     C+ CLOSE C5b
081200091127     C/END-EXEC
081300091127     c                   endsl
081400091127
081500091127     c                   if        wgiro='1'
081600091127     c                   if        wcontcli <=§vponclimx
081700091127     c                   eval      wgiro='2'
081800091127     c                   goto      I_strad5
081900091127     c                   else
082000091127     c* passo al chiamante il numero dei clienti elaborabili per
082100091127     c* l'emissione del msg di errore
082200091127     c                   if        wcontcli >= 99999
082300091127     c                   move      '99999'       p_numcli
082400091127     c                   else
082500091127     c                   move      wcontcli      p_numcli
082600091127     c                   endif
082700091127     c                   endif
082800091127     c                   else
082900091127     c                   clear                   p_numcli
083000091127     c                   endif
083100091127      *
083200091127     C                   ENDSR
083300091002
083400091001      *-----------------------------------------------------------------------*
083500091002      * Stringa sql per clienti
083600091001      *-----------------------------------------------------------------------*
083700091002     c     sr_sql1       begsr
083800091002
083900091002     c                   clear                   StringaSql
084000091001
084100091001     c                   eval      StringaSql =
084200091001     c                             'select * -
084300091002     c                             from cnaco00f, cnind00f, cnclp00f -
084400091002     c                             where acokut = indkut and acokut = -
084500091002     c                             clpkut and acokcc = indkcc and -
084600091002     c                             acokcc = clpkcc and acoksc = -
084700091002     c                             indksc and acoksc = clpksc and -
084800091002     c                             acokut = 1 and acokcc = 151 and -
084900091006     c                             acoflg <> ''A'' '
085000091002      * è richiesto un solo cliente
085100091002     c                   if        i45ksc <> 0
085200091002     c                   eval      StringaSql = %trim(StringaSql) +
085300091006     c                             ' and acoksc = ' +
085400091002     c                             %editc(i45ksc:'X')
085500091002     c                   goto      $dopo1
085600091002     c                   endif
085700091002
085800091002      * più clienti
085900091002     c                   eval      StringaSql = %trim(StringaSql) +
086000091006     c                             ' and substr(digits(acoksc), 4, 4) <> -
086100091002     c                              ''88888'' and substr(digits(-
086200091002     c                              acoksc), 4, 4) <> ''9999'' '
086300091001
086400091002      * se sono in filiale escludo i clienti non gestibili
086500091002      * se non richiesta 1 sola filiale
086600091002     c                   if        simfel <> 0 and i45app = 0
086700091002     c                   eval      StringaSql = %trim(StringaSql) +
086800091002     c                             ' and substr(digits(acoksc), 1, 3) -
086900091002     c                             in ('
087000091002     c                   do        250           xa
087100091002     c                   if        pog(xa) = *zeros
087200091002     c                   leave
087300091002     c                   endif
087400091002     c                   eval      StringaSql =
087500091001     c                             %trim(StringaSql) +  ' ' +
087600091002     c                             %editc(pog(xa):'X') + ','
087700091002     c                   enddo
087800091002     c                   eval      StringaSql =
087900091002     c                             %trim(StringaSql) + ')'
088000091002     c                   endif
088100091002
088200091002      * è richiesto un commerciale
088300091002     c                   if        i45cmm <> 0
088400091002     c                   eval      StringaSql =
088500091002     c                             %trim(StringaSql) +
088600091002     c                             ' and clpage = ' +
088700091002     c                             %editc(i45cmm:'X')
088800091002     c                   endif
088900091002      * è richiesta la categoria
089000091002     c                   if        i45sct <> 0
089100091002     c                   eval      StringaSql =
089200091002     c                             %trim(StringaSql) +
089300091002     c                             ' and acoitc = ' +
089400091002     c                             %editc(i45sct:'X')
089500091002     c                   endif
089600091002      * è richiesto il codice importanza clienti
089700091002     c                   if        i45clv <> *blanks
089800091002     c                   eval      StringaSql =
089900091002     c                             %trim(StringaSql) +
090000091002     c                             ' and clpclv = ' + '''' +
090100091002     c                              i45clv + ''''
090200091002     c                   endif
090300091002      * è richiesta una filiale
090400091002     c                   if        i45app <> 0
090500091002     c                   eval      StringaSql =
090600091002     c                             %trim(StringaSql) + ' and -
090700091002     c                             substr(digits(acoksc), 1, 3) = '
090800091002     c                             + %editc(i45app:'X')
090900091002     c                   endif
091000091002      * è richiesta una provincia
091100091002     c                   if        i45pro <> *blanks
091200091002     c                   eval      StringaSql =
091300091002     c                             %trim(StringaSql) +
091400091002     c                             ' and indprv = ' +
091500091002     c                             '''' + i45pro + ''''
091600091002     c                   endif
091700091002      * è richiesto un CAP
091800091002     c                   if        i45cap <> *blanks
091900091002     c                   eval      StringaSql =
092000091002     c                             %trim(StringaSql) +
092100091002     c                             ' and indcae = ' +
092200091002     c                             '''' + i45cap + ''''
092300091002     c                   endif
092400091002      * è richiesta una nazione
092500091002     c                   if        i45nar <> '999'
092600091002     c                   eval      StringaSql =
092700091002     c                             %trim(StringaSql) +
092800091002     c                             ' and indsta = ' +
092900091002     c                             '''' + i45nar + ''''
093000091002     c                   endif
093100091002      * solo clienti bloccati
093200091002     c                   if        i45abl = 'B'
093300091002     c                   eval      StringaSql =
093400091002     c                             %trim(StringaSql) +
093500091002     c                             ' and acoabl <> '' '''
093600091002     c                   endif
093700091002      * solo clienti NON bloccati
093800091002     c                   if        i45abl = 'N'
093900091002     c                   eval      StringaSql =
094000091002     c                             %trim(StringaSql) +
094100091002     c                             ' and acoabl = '' '''
094200091002     c                   endif
094300091002
094400091002     c     $dopo1        tag
094500091002
094600091001     c                   Eval      StringaSql =
094700091002     c                             %trim(StringaSql) + ' for read only'
094800091001
094900091001     C/EXEC SQL
095000091001     C+ prepare S1 from :StringaSql
095100091001     C/END-EXEC
095200091001
095300091001     C/EXEC SQL
095400091001     C+ declare C1 cursor for S1
095500091001     C/END-EXEC
095600091001
095700091001     c                   endsr
095800091127
095900091127      *-----------------------------------------------------------------------*
096000091127      * Stringa sql per trattative
096100091127      *-----------------------------------------------------------------------*
096200091127     c     sr_sql5       begsr
096300091127
096400091127     c                   clear                   StringaSql
096500091127
096600091127      * sql con trattative e anagragriche
096700091127     c                   if        $visaco
096800091127
096900091127     c                   eval      StringaSql =
097000091127     c                             'select * -
097100091127     c                              from tivis00f, tfaco00f, tfind00f, -
097200091127     c                              tfclp00f -
097300091127     c                              where visnrv = acoksc and -
097400091127     c                              acokut = indkut and acokut = -
097500091127     c                              clpkut and acokcc = indkcc and -
097600091127     c                              acokcc = clpkcc and acoksc = -
097700091127     c                              indksc and acoksc = clpksc and -
097800091127     c                              acokut = 1 and -
097900091127     c                              acoflg <> ''A'' and visdch = 0 -
098000091127     c                              and visksc = 0 '
098100091127      * è richiesta una sola visita
098200091127     c                   if        i45nrv <> 0
098300091127     c                   eval      StringaSql = %trim(StringaSql) +
098400091127     c                             ' and visnrv = ' +
098500091127     c                             %editc(i45nrv:'X')
098600091127     c                   goto      $dopo5a
098700091127     c                   endif
098800091127
098900091127      * più visite
099000091127      * è richiesto un commerciale
099100091127     c                   if        i45cmm <> 0
099200091127     c                   eval      StringaSql =
099300091127     c                             %trim(StringaSql) +
099400091127     c                             ' and viscmm = ' +
099500091127     c                             %editc(i45cmm:'X')
099600091127     c                   endif
099700091127      * è richiesto il tipo visita
099800091127     c                   if        i45tpv <> *blanks
099900091127     c                   eval      StringaSql =
100000091127     c                             %trim(StringaSql) +
100100091127     c                             ' and vistpv = ' +
100200091127     c                             i45tpv
100300091127     c                   endif
100400091127      * è richiesta una filiale
100500091127     c                   if        i45app <> 0
100600091127     c                   eval      StringaSql =
100700091127     c                             %trim(StringaSql) + ' and -
100800091127     c                             substr(digits(viscmm), 1, 3) = '
100900091127     c                             + %editc(i45app:'X')
101000091127     c                   endif
101100091127      * è richiesta la categoria
101200091127     c                   if        i45sct <> 0
101300091127     c                   eval      StringaSql =
101400091127     c                             %trim(StringaSql) +
101500091127     c                             ' and acoitc = ' +
101600091127     c                             %editc(i45sct:'X')
101700091127     c                   endif
101800091127      * è richiesto il codice importanza clienti
101900091127     c                   if        i45clv <> *blanks
102000091127     c                   eval      StringaSql =
102100091127     c                             %trim(StringaSql) +
102200091127     c                             ' and clpclv = ' + '''' +
102300091127     c                              i45clv + ''''
102400091127     c                   endif
102500091127      * è richiesta una provincia
102600091127     c                   if        i45pro <> *blanks
102700091127     c                   eval      StringaSql =
102800091127     c                             %trim(StringaSql) +
102900091127     c                             ' and indprv = ' +
103000091127     c                             '''' + i45pro + ''''
103100091127     c                   endif
103200091127      * è richiesto un CAP
103300091127     c                   if        i45cap <> *blanks
103400091127     c                   eval      StringaSql =
103500091127     c                             %trim(StringaSql) +
103600091127     c                             ' and indcae = ' +
103700091127     c                             '''' + i45cap + ''''
103800091127     c                   endif
103900091127      * è richiesta una nazione
104000091127     c                   if        i45nar <> '999'
104100091127     c                   eval      StringaSql =
104200091127     c                             %trim(StringaSql) +
104300091127     c                             ' and indsta = ' +
104400091127     c                             '''' + i45nar + ''''
104500091127     c                   endif
104600091127
104700091127     c     $dopo5a       tag
104800091127
104900091127     c                   Eval      StringaSql =
105000091127     c                             %trim(StringaSql) + ' for read only'
105100091127
105200091127     C/EXEC SQL
105300091127     C+ prepare S5a from :StringaSql
105400091127     C/END-EXEC
105500091127
105600091127     C/EXEC SQL
105700091127     C+ declare C5a cursor for S5a
105800091127     C/END-EXEC
105900091127
106000091127     c                   endif
106100091127
106200091127      * sql solo trattative
106300091127     c                   if        not $visaco
106400091127
106500091127     c                   eval      StringaSql =
106600091127     c                             'select * -
106700091127     c                              from tivis00f where visdch = 0'
106800091127      * è richiesta una sola visita
106900091127     c                   if        i45nrv <> 0
107000091127     c                   eval      StringaSql = %trim(StringaSql) +
107100091127     c                             ' and visnrv = ' +
107200091127     c                             %editc(i45nrv:'X')
107300091127     c                   goto      $dopo5b
107400091127     c                   endif
107500091127
107600091127      * più visite
107700091127      * è richiesto un commerciale
107800091127     c                   if        i45cmm <> 0
107900091127     c                   eval      StringaSql =
108000091127     c                             %trim(StringaSql) +
108100091127     c                             ' and viscmm = ' +
108200091127     c                             %editc(i45cmm:'X')
108300091127     c                   endif
108400091127      * è richiesto il tipo visita
108500091127     c                   if        i45tpv <> *blanks
108600091127     c                   eval      StringaSql =
108700091127     c                             %trim(StringaSql) +
108800091127     c                             ' and vistpv = ' +
108900091127     c                             i45tpv
109000091127     c                   endif
109100091127      * è richiesta una filiale
109200091127     c                   if        i45app <> 0
109300091127     c                   eval      StringaSql =
109400091127     c                             %trim(StringaSql) + ' and -
109500091127     c                             substr(digits(viscmm), 1, 3) = '
109600091127     c                             + %editc(i45app:'X')
109700091127     c                   endif
109800091127
109900091127     c     $dopo5b       tag
110000091127
110100091127     c                   Eval      StringaSql =
110200091127     c                             %trim(StringaSql) + ' for read only'
110300091127
110400091127     C/EXEC SQL
110500091127     C+ prepare S5b from :StringaSql
110600091127     C/END-EXEC
110700091127
110800091127     C/EXEC SQL
110900091127     C+ declare C5b cursor for S5b
111000091127     C/END-EXEC
111100091127
111200091127     c                   endif
111300091127
111400091127     c                   endsr
111500091001
111600961203      *************************************************************************
111700041029      * ROUTINE DI CONTROLLO SE INVIO TRAMITE E-MAIL/FAX OPPURE NO
111800961203      *************************************************************************
111900961203     C     CTRFAX        BEGSR
112000050912     c     w45htm        ifeq      *blanks
112100041029     C                   SETOFF                                       0809
112200060919     c                   eval      w_noelab=' '
112300041102    1c     i45fax        ifeq      'S'
112400060919     c     i45fax        oreq      'M'
112500041029     c* verifico se esiste indirizzo e-mail
112600050908     c                   movel     '06'          ktnt
112700050908     c     kntc          chain     tfntc01l
112800060919     c* se visita e non trovata nota per numero visita la cerco con cliente
112900060919     c* se presente
113000110204     c                   if        i45ast = 'T' and
113100091022     c                             (not %Found(tfntc01l) or
113200060919     c                             ntcrnt=*blanks) and visksc>0
113300060919     c                   movel     'C'           kapl
113400060919     c                   movel     i45kcc        knk1
113500060919     c                   move      visksc        knk1
113600060919     c     kntc          chain     tfntc01l
113700060919     c                   endif
113800041102    2c                   if        %found(tfntc01l)
113900041029     c                             and ntcrnt <> *blanks
114000041029     c* eseguo override
114100041029     c                   exsr      ovrmail
114200041102    3c     d44err        ifeq      *blanks
114300041102     c                   seton                                        09
114400041102    3c                   endif
114500091127    2c                   endif
114600060919    1c                   endif
114700060919    1c                   select
114800060919     c                   when      i45fax='S'
114900041102    2c     *in09         ifeq      *off
115000041029     c* indirizzo e-mail non trovato o campo vuoto verifico numero fax
115100041102    3c                   if        indtlf <> *blanks
115200961203     C*
115300961203     C* SE INVIO TRAMITE FAX CONTROLLO VALIDITA' NUMERO FAX
115400050908     C                   CLEAR                   TRUL42DS
115500961203     C                   MOVEL     INDTLF        D42FAX
115600961203     C                   CALL      'TRUL42R'
115700021025     C                   PARM                    TRUL42DS
115800961203     C* 08 ON  - FAX VALIDO: STAMPO NELLO SPOOL PER INVIO FAX
115900041102    4C     D42ERR        IFEQ      ' '
116000961203     C                   SETON                                        08
116100041102    4C                   ENDIF
116200041102    3C                   ENDIF
116300041102    2C                   ENDIF
116400060919     c                   when      i45fax='M'  and *in09=*off
116500060919     c                   eval      W_noelab='1'
116600060919    1C                   ENDsl
116700050908     c                   else
116800050908     c                   exsr      ovrhtml
116900050908     c* se ho trovato e-mail cerco anche nominativo cliente
117000050908     c                   move      i45nmc        w45nmc
117100050908     c                   if        i45nmc = *blanks and d44err=*blanks
117200050908     c                   movel     '05'          ktnt
117300050908     c     kntc          chain     tfntc01l
117400050908     c                   if        %found(tfntc01l)
117500050908     c                             and ntcrnt <> *blanks
117600050908     c                   eval      w45nmc = ntcrnt
117700050908     c                   else
117800050909     c                   eval      w45nmc = 'Responsabile Trasporti'
117900050908     c                   endif
118000050908     c                   endif
118100050908     c                   endif
118200961203     C*
118300961203     C                   ENDSR
118400000000      *************************************************************************
118500000000      * INTESTAZIONE LETTERA
118600000000      *************************************************************************
118700000000     C     TESLET        BEGSR
118800000000      *
118900000000      * Inizializzo il numero delle pagine
119000000000     C                   Z-ADD     1             $PAGIN            4 0
119100000000      *
119200000000      * DECODIFICA DATI ANAGRAFICI
119300000000     C                   EXSR      DECANA
119400940330      *
119500940330      * Compongo la localita'
119600950201     C     WNAR          IFEQ      *BLANKS
119700950201     C* SE NAZIONE E' *BLANK=ITALIA NON LA STAMPO QUINDI STAMPO IL
119800950201     C*   CAP + LOCALITA' + PROV. SU UNA SOLA RIGA
119900960426     C     *IN33         IFEQ      *ON
120000960426     C* 33 ON  - TESTO IN COMPRESSO
120100960426     C                   MOVEL     INDCIT        $COMOD           30
120200960426     C                   MOVE      *BLANKS       LOCALP           34
120300940330     C     $COMOD        CAT       INDPRV:2      LOCALP
120400960426     C                   ELSE
120500960426     C* 33 OFF - TESTO IN NORMALE
120600960426     C                   MOVEL     WCAP          WCAP5             5
120700960426     C                   MOVEL     INDCIT        $COM21           21
120800960426     C                   MOVE      *BLANKS       LOCAL2           25
120900960426     C     $COM21        CAT       INDPRV:2      LOCAL2
121000960426     C                   ENDIF
121100950201     C*
121200950201     C                   ELSE
121300950201     C* SE NAZIONE ESTERA STAMPO  CAP + LOCALITA' SU UNA RIGA; STAMPO
121400960429     C*   NAZIONE + DECODIFICA NAZIONE  NELLA RIGA SOTTO.
121500960429     C*   SE ESTERO NON STAMPO LA PROVINCIA
121600960429     C  N33              MOVEL     INDCIT        LOCAL3           21
121700950201     C                   ENDIF
121800000000      *
121900000000      * VALORIZZO DATI FILIALE E DATA SOLO PER FV1 E FV6 = 'S'
122000000000     C     *IN21         IFEQ      '0'
122100000000     C     *IN26         OREQ      '0'
122200000000     C     *IN27         OREQ      '0'
122300120611     c                   eval      wpo = $filia
122400120611     c                   exsr      ctrpo
122500120611     c                   eval      $filia = wpo
122600000000     C     $FILIA        CHAIN     AZORG01L                           35
122700000000      *
122800000000     C     *IN27         IFEQ      '0'
122900000000     C  N35              MOVEL     ORGDES        TD7FIL           20
123000000000     C   35              MOVEL     *BLANKS       TD7FIL
123100000000     C                   ENDIF
123200000000      *
123300000000     C     *IN21         IFEQ      '0'
123400000000     C  N35              MOVEL     ORGDES        TD1FIL           20
123500000000     C   35              MOVEL     *BLANKS       TD1FIL
123600000000      *
123700000000     C                   MOVEL     ORGIND        TD1VIA           25
123800000000     C                   MOVEL     ORGCPF        TD1CAP            5 0
123900940315     C                   MOVE      *BLANKS       TD1LOC           24
124000940315     C     ORGLOC        CAT       ORGPRO:2      TD1LOC
124100000000     C                   MOVEL     ORGTEL        TD2TEL           13
124200000000     C                   MOVEL     ORGFAX        TD2TFX           13
124300000000     C                   ENDIF
124400000000      *
124500960910     C* COSTRUISCO LOCALITA' + DATA
124600060609     C                   CLEAR                   LOCDAT           35
124700060609     C                   MOVEL     I45GDS        WDATA
124800060609      *
124900060609     c                   if        $codlin = 'I'
125000060609     c                   eval      locdat = orgdes + ', li '+
125100060609     c                             wgg + '/' + wmm + '/' + waa
125200060609     c                   else
125300060609     c                   eval      locdat = orgdes + ', ' +
125400060609     c                             wgg + '/' + wmm + '/' + waa
125500060609     c                   endif
125600000000     C                   ENDIF
125700000000      *
125800000000      * CONTROLLO ESISTENZA RIFERIMENTI, SOLO SE RICHIESTI
125900021022     C     §TXSRI        IFEQ      'S'
126000981113     C     I45NR1        COMP      *BLANKS                            61
126100981113     C  N61I45NR2        COMP      *BLANKS                            61
126200000000     C                   ELSE
126300000000     C                   SETOFF                                       61
126400000000     C                   ENDIF
126500060608
126600060608      *? CHIODO ATTENZIONE!!!!! ?
126700060608      * purtroppo devo chiodare la stampa delle costanti in lingua
126800060608      * il controllo lo faccio con il campo chiave che ho usato per
126900060608      * reperire il testo
127000060608      * ITALIANO
127100060608     c                   if        $codlin = 'I'
127200070319     c                   eval      let01 = 'Filiale di:'
127300060608     c                   eval      let02 = 'Spett.le'
127400060608     c                   eval      let03 = 'Tel. '
127500060608     c                   eval      let04 = 'Fax  '
127600060608     c                   eval      let05 = 'ns. rif.'
127700110209     c                   eval      let05 = %trim(let05) + ' ' +
127800060608     c                             i45nr1 + ' / ' + i45nr2
127900060608     c                   evalr     let06 = 'C.A.'
128000060608     c  n98              eval      let07 = 'Telefonando chiedere del Sig.'
128100060608     c  n98              eval      let07 = %trim(let07) + ' ' + descom
128200060608     c   98              eval      let07 = 'Telefonando chiedere dell''Uf-
128300060608     c                             ficio Commerciale'
128400070319     c                   eval      let08 = 'Filiale di'
128500060612     c                   eval      let08 = %trim(let08) + ' ' + td7fil
128600060608     c                   endif
128700060608      * INGLESE
128800060608     c                   if        $codlin = '2'
128900070319     c                   eval      let01 = 'Branch:'
129000060608     c                   eval      let02 = 'Messrs.'
129100060608     c                   eval      let03 = 'Tel. '
129200060608     c                   eval      let04 = 'Fax  '
129300060608     c                   eval      let05 = 'our ref.'
129400110209     c                   eval      let05 = %trim(let05) + ' ' +
129500060608     c                             i45nr1 + ' / ' + i45nr2
129600060608     c                   evalr     let06 = 'For the attention of'
129700060608     c  n98              eval      let07 = 'When phoning, please ask for Mr.'
129800060608     c  n98              eval      let07 = %trim(let07) + ' ' + descom
129900060608     c   98              eval      let07 = 'Telefonando chiedere dell''Uf-
130000060608     c                             ficio Commerciale'
130100070319     c                   eval      let08 = 'Branch'
130200060612     c                   eval      let08 = %trim(let08) + ' ' + td7fil
130300060608     c                   endif
130400060608      * FRANCESE
130500060608     c                   if        $codlin = '3'
130600070319     c                   eval      let01 = 'Filiale de:'
130700060608     c                   eval      let02 = 'Société'
130800060608     c                   eval      let03 = 'Tél. '
130900060608     c                   eval      let04 = 'Fax  '
131000060608     c                   eval      let05 = 'notre réf.'
131100110209     c                   eval      let05 = %trim(let05) + ' ' +
131200060608     c                             i45nr1 + ' / ' + i45nr2
131300060608     c                   evalr     let06 = 'A l''attention de'
131400060608     c  n98              eval      let07 = 'Au téléphone, demander M.'
131500060608     c  n98              eval      let07 = %trim(let07) + ' ' + descom
131600060608     c   98              eval      let07 = 'Telefonando chiedere dell''Uf-
131700060608     c                             ficio Commerciale'
131800070319     c                   eval      let08 = 'Filiale de'
131900060612     c                   eval      let08 = %trim(let08) + ' ' + td7fil
132000060608     c                   endif
132100060608      * TEDESCO
132200060608     c                   if        $codlin = '4'
132300070319     c                   eval      let01 = 'Filiale in:'
132400060608     c                   eval      let02 = 'An'
132500060608     c                   eval      let03 = 'Tel. '
132600060608     c                   eval      let04 = 'Fax  '
132700060608     c                   eval      let05 = 'unser zeichen'
132800110209     c                   eval      let05 = %trim(let05) + ' ' +
132900060608     c                             i45nr1 + ' / ' + i45nr2
133000060608     c                   evalr     let06 = 'Zu Hd.'
133100060608     c  n98              eval      let07 = 'Telefonischer Ansprechpartner: Herr'
133200060608     c  n98              eval      let07 = %trim(let07) + ' ' + descom
133300060608     c   98              eval      let07 = 'Telefonando chiedere dell''Uf-
133400060608     c                             ficio Commerciale'
133500070319     c                   eval      let08 = 'Filiale in'
133600060612     c                   eval      let08 = %trim(let08) + ' ' + td7fil
133700060608     c                   endif
133800000000      *
133900000000      * STAMPA TESTATA
134000041029     c                   select
134100041029     c     *in08         wheneq    *off
134200041029     c     *in09         andeq     *off
134300041029     c                   except    testa
134400041029     c     *in09         wheneq    *on
134500041029     c   33              except    testafm
134600041029     c  n33              except    testanm
134700041029     c     *in08         wheneq    *on
134800041029     c   33              except    testaf
134900041029     c  n33              except    testan
135000041029     c                   endsl
135100000000      *
135200000000     C                   ENDSR
135300000000      *************************************************************************
135400000000      * ROUTINE DI STAMPA CORPO LETTERA
135500000000      *************************************************************************
135600000000     C     CORLET        BEGSR
135700000000      *
135800021025     c                   eval      *in01 = *off
135900021025     c                   eval      *in02 = *off
136000021025
136100021022     C     KTxt          SETLL     FnTxt01l
136200000000      *
136300000000DO  1C                   DO        *HIVAL
136400021022     C     KTxt          READE     FnTxt01l                               31
136500021022      *
136600021022      * Salto il record se annullato o se tipo H
136700021022IF  2C  N31              IF        TxtAtb <>'A' and TxtPos <>'H'
136800000000      *
136900000000      * MEMORIZZO SE CI SONO RIGHE IN CALCE
137000000000IF  3C     TXTPOS        IFEQ      'F'
137100000000     C     TXTNRR        IFEQ      1
137200000000     C                   MOVEL     TXTRIG        CAL1            110
137300000000     C                   MOVEL     TXTRIB        CALRB1            1
137400021025     C                   eval      *in01 = *on
137500000000     C                   ELSE
137600000000     C                   MOVEL     TXTRIG        CAL2            110
137700000000     C                   MOVEL     TXTRIB        CALRB2            1
137800021025     C                   eval      *in02 = *on
137900000000     C                   END
138000000000      *
138100000000X   3C                   ELSE
138200000000      *
138300000000      * SE NON HO LETTO RIGHE DI CALCE INIZIO A STAMPARE
138400000000      * STAMPO RIGA + SALTO PAGINA
138500961129    4C     TXTPOS        IFEQ      'S'
138600021025      *
138700021025    5c                   if        *in08 = *off
138800041029     c                             and *in09 = *off
138900021025     C                   EXCEPT    RIG
139000021025     c                   else
139100041029     c                   if        *in09 = *on
139200041029     C   33              EXCEPT    RIGFM
139300041029     C  N33              EXCEPT    RIGNM
139400041029     c                   else
139500021025     C   33              EXCEPT    RIGF
139600021025     C  N33              EXCEPT    RIGN
139700021025    5c                   endif
139800041029    5c                   endif
139900021025      *
140000000000      * SE RIBATTITURA RISTAMPA
140100961129    5C     TXTRIB        IFEQ      'R'
140200021025    6c                   if        *in08 = *off
140300041029     c                             and *in09 = *off
140400021025     C                   EXCEPT    RIG2
140500021025     c                   else
140600041029     c                   if        *in09 = *on
140700041029     C   33              EXCEPT    RIG2FM
140800041029     C  N33              EXCEPT    RIG2NM
140900041029     c                   else
141000021025     C   33              EXCEPT    RIG2F
141100021025     C  N33              EXCEPT    RIG2N
141200021025    6C                   endif
141300041029    6C                   endif
141400021025    5C                   ENDIF
141500021025      *
141600021025    5c                   IF        *in08 = *off
141700041029     c                             and *in09 = *off
141800021025     C                   eval        *inOF = *ON
141900021025     c                   ELSE
142000041029     c                   if        *in09 = *on
142100041029     C   33              eval      *inOC = *on
142200041029     C  N33              eval      *inOD = *on
142300041029     c                   else
142400021025     C   33              eval      *inOG = *on
142500021025     C  N33              eval      *inOE = *on
142600021025    5C                   ENDIF
142700041029    5C                   ENDIF
142800021025    4C                   ENDIF
142900000000      *
143000961129      * PER "OF"/"OG" STAMPO 1 RIGA IN CALCE SE C'E'
143100970326    4C     *IN08         IFEQ      *OFF
143200041029     c     *in09         andeq     *off
143300961129     C     *INOF         ANDEQ     *ON
143400970326     C*
143500970326     C     *IN08         OREQ      *ON
143600961129     C     *INOG         ANDEQ     *ON
143700970326     C     *IN33         ANDEQ     *ON
143800970326     C*
143900970326     C     *IN08         OREQ      *ON
144000970326     C     *INOE         ANDEQ     *ON
144100970326     C     *IN33         ANDEQ     *OFF
144200970326     C* STAMPO RIGHE IN CALCE
144300970326     C                   EXSR      CALCE
144400000000      *
144500000000      * VADO A SALTO PAGINA (NUMEARNDO, SE RICHIESTO)
144600970318     C   08
144700970318     CORN28              DO
144800041103     C  N09              ADD       1             $PAGIN
144900970326     C   33
145000970326     CAN 08              EXCEPT    PAG2
145100970326     C  N33
145200970326     CAN 08              EXCEPT    PAG2N
145300000000     C                   ENDDO
145400041029     c  N09
145500041029     CanN08              EXCEPT    PAG3
145600970326     C   33
145700970326     CAN 08              EXCEPT    PAG3F
145800970326     C  N33
145900970326     CAN 08              EXCEPT    PAG3N
146000041029     C  n09
146100041029     CANN08              SETOFF                                       OF
146200041029     C   33
146300041029     CAN 09              SETOFF                                       OC
146400041029     C  N33
146500041029     CAN 09              SETOFF                                       OD
146600970326     C   33
146700970326     CAN 08              SETOFF                                       OG
146800970326     C  N33
146900970326     CAN 08              SETOFF                                       OE
147000961203    4C                   ENDIF
147100000000      *
147200000000      * STAMPO RIGA SE RIGA DI LETTERA NORMALE
147300000000     C     TXTPOS        IFEQ      'C'
147400041029     c  N09
147500041029     CANN08              EXCEPT    RIG
147600041029     C   33
147700041029     CAN 09              EXCEPT    RIGFM
147800041029     C  N33
147900041029     CAN 09              EXCEPT    RIGNM
148000970326     C   33
148100970326     CAN 08              EXCEPT    RIGF
148200970326     C  N33
148300970326     CAN 08              EXCEPT    RIGN
148400000000      * SE RIBATTITURA RISTAMPA
148500000000     C     TXTRIB        IFEQ      'R'
148600041029     c  N09
148700041029     CANN08              EXCEPT    RIG2
148800041029     C   33
148900041029     CAN 09              EXCEPT    RIG2FM
149000041029     C  N33
149100041029     CAN 09              EXCEPT    RIG2NM
149200970326     C   33
149300970326     CAN 08              EXCEPT    RIG2F
149400970326     C  N33
149500970326     CAN 08              EXCEPT    RIG2N
149600000000     C                   ENDIF
149700000000     C                   ENDIF
149800000000      *
149900000000E   3C                   ENDIF
150000000000      *
150100000000E   2C                   ENDIF
150200000000E   1C  N31              ENDDO
150300000000      *
150400000000     C                   ENDSR
150500970326      *************************************************************************
150600970326      * STAMPO RIGHE IN CALCE
150700970326      *************************************************************************
150800970326     C     CALCE         BEGSR
150900970326     C     *IN02         IFEQ      *ON
151000041029     c  N09
151100041029     CANN08              EXCEPT    STCA2
151200041029     C   33
151300041029     CAN 09              EXCEPT    STCA2FM
151400041029     C  N33
151500041029     CAN 09              EXCEPT    STCA2NM
151600970326     C   33
151700970326     CAN 08              EXCEPT    STCA2F
151800970326     C  N33
151900970326     CAN 08              EXCEPT    STCA2N
152000970326     C     CALRB2        IFEQ      'R'
152100041029     c  N09
152200041029     CANN08              EXCEPT    STCAR2
152300041029     C   33
152400041029     CAN 09              EXCEPT    STCR2FM
152500041029     C  N33
152600041029     CAN 09              EXCEPT    STCR2NM
152700970326     C   33
152800970326     CAN 08              EXCEPT    STCR2F
152900970326     C  N33
153000970326     CAN 08              EXCEPT    STCR2N
153100970326     C                   ENDIF
153200970326     C                   ENDIF
153300970326     C*
153400970326      * STAMPO SECONDA RIGA IN CALCE SE C'E'
153500970326     C     *IN01         IFEQ      *ON
153600041029     c  N09
153700041029     CANN08              EXCEPT    STCA1
153800041126     C   33
153900041126     CAN 09
154000041126     CAN 02              EXCEPT    VUO1FM
154100041126     C   33
154200041126     CAN 09
154300041126     CANN02              EXCEPT    VUO2FM
154400041029     C   33
154500041029     CAN 09              EXCEPT    STCA1FM
154600041126     C  N33
154700041126     CAN 09
154800041126     CAN 02              EXCEPT    VUO1NM
154900041126     C  N33
155000041126     CAN 09
155100041126     CANN02              EXCEPT    VUO2NM
155200041029     C  N33
155300041029     CAN 09              EXCEPT    STCA1NM
155400970326     C   33
155500970326     CAN 08              EXCEPT    STCA1F
155600970326     C  N33
155700970326     CAN 08              EXCEPT    STCA1N
155800970326     C     CALRB1        IFEQ      'R'
155900041029     c  N09
156000041029     CANN08              EXCEPT    STCAR1
156100041029     C   33
156200041029     CAN 09              EXCEPT    STCR1FM
156300041029     C  N33
156400041029     CAN 09              EXCEPT    STCR1NM
156500970326     C   33
156600970326     CAN 08              EXCEPT    STCR1F
156700970326     C  N33
156800970326     CAN 08              EXCEPT    STCR1N
156900970326     C                   ENDIF
157000970326     C                   ENDIF
157100970326     C                   ENDSR
157200000000      *************************************************************************
157300000000      * PARTE FINALE DELLA LETTERA
157400000000      *************************************************************************
157500000000     C     FINLET        BEGSR
157600000000      *
157700000000      * STAMPO FIRMA IN FONDO ALLA LETTERA
157800041029     C  N09
157900041029     CANN08              EXCEPT    FIRMA
158000041029     C   33
158100041029     CAN 09              EXCEPT    FIRMAFM
158200041029     C  N33
158300041029     CAN 09              EXCEPT    FIRMANM
158400970326     C   33
158500970326     CAN 08              EXCEPT    FIRMAF
158600970326     C  N33
158700970326     CAN 08              EXCEPT    FIRMAN
158800970326     C* STAMPO RIGHE IN CALCE
158900970326     C                   EXSR      CALCE
159000940330     C*
159100940330     C                   MOVE      *BLANKS       LOCALP
159200000000      *
159300000000     C                   ENDSR
159400000000      *************************************************************************
159500000000      * ROUTINE DI DECODIFICA DATI ANAGRAFICI
159600000000      *************************************************************************
159700000000     C     DECANA        BEGSR
159800000000      *
159900000000     C                   SETOFF                                       607374
160000940331     C                   SETOFF                                       616263
160100970225     C                   SETOFF                                       1698
160200000000     C                   MOVEL     *BLANKS       DESRS1           44
160300050224     C                   MOVEL     *BLANKS       DESRS2           44
160400000000     C                   MOVE      *BLANKS       DESCOM           20
160500000000      *
160600021022      * COMMERCIALE (PER §TXSTC='S')
160700940330    1C     *IN25         IFEQ      *OFF
160800981113    2C     I45AST        IFNE      'L'
160900940330     C*
161000940330    3C     CLPAGE        IFNE      0
161100000000     C                   Z-ADD     1             TBLKUT
161200000000     C                   MOVE      '01'          TBLCOD
161300000000     C                   MOVE      *BLANKS       TBLKEY
161400000000     C                   MOVEL     CLPAGE        TBLKEY
161500000000     C     TABELK        CHAIN     TABEL00F                           35
161600000000     C  N35              MOVEL     TBLUNI        DESCOM
161700000000     C   35              MOVE      *BLANKS       DESCOM
161800000000     C                   SETON                                        60
161900940330    3C                   ENDIF
162000970225     C* SE TESTO SEDE LB STAMPO ALLA CA UFFICIO COMMERICALE
162100970225     C     SIMFEL        IFEQ      0
162200981113     C     I45TTX        ANDEQ     'LB'
162300970225     C                   SETON                                        98
162400970225     C                   ENDIF
162500940330     C*
162600940330   X2C                   ELSE
162700940330     C*
162800940330     C* SE E' STATO PASSATO LO SCRIVO
162900981113    3C     I45DCM        IFNE      *BLANKS
163000940330     C                   SETON                                        60
163100981113     C                   MOVEL     I45DCM        DESCOM
163200940330    3C                   ENDIF
163300940330    2C                   ENDIF
163400940330    1C                   ENDIF
163500940531      *-------------------------------------------------------------------------
163600021022      * ALLA CORTESE ATTENZIONE (PER §TXSCA='S')
163700940531      * RESPONSABILE TRASPORTI
163800000000IF  1C     *IN24         IFEQ      '0'
163900940330     C*
164000981113IF  2C     I45AST        IFEQ      'C'
164100091022     C     I45AST        OREQ      'T'
164200940601     C                   SETOFF                                       36
164300940531      *
164400981113IF  3C     I45AST        IFEQ      'C'
164500940601     C                   MOVE      ACOKSC        KEY1C
164600940601     C                   MOVE      *BLANKS       NOTNK2
164700010131     C                   SETOFF                                       35
164800010131     C     TFNT2K        SETLL     TFNTC01L
164900010131     C                   DO        *HIVAL
165000010131     C     TFNT2K        READE     TFNTC01L                               38
165100010131     C   38              LEAVE
165200010131     C     NTCFLT        IFNE      'A'
165300010131     C     TFNT2K        SETLL     TFNTC01L                               35
165400010131     C                   LEAVE
165500010131     C                   ENDIF
165600010131     C                   ENDDO
165700940601E   3C                   ENDIF
165800940601      *
165900110204IF  3C     I45AST        ifeq      'T'
166000940601      * PER PRIMA COSA CERCO LE NOTE PER VISITA
166100940601     C                   MOVE      VISNRV        NOTNK1
166200940601     C                   MOVE      *BLANKS       NOTNK2
166300010131     C                   SETOFF                                       35
166400010131     C     TFNTCK        SETLL     TFNTC01L
166500010131     C                   DO        *HIVAL
166600010131     C     TFNTCK        READE     TFNTC01L                               38
166700010131     C   38              LEAVE
166800010131     C     NTCFLT        IFNE      'A'
166900010131     C     TFNTCK        SETLL     TFNTC01L                               35
167000010131     C                   LEAVE
167100010131     C                   ENDIF
167200010131     C                   ENDDO
167300940601     C   35              SETON                                        36
167400940601     C* SE NON TROVATA ED E' VISITA DI CLIENTE CODIFICATO CERCO
167500940601     C*  CON CODICE CLIENTE
167600940601IF  4C  N35VISKSC        IFNE      0
167700940601     C                   MOVE      VISKSC        KEY1C
167800940531     C                   MOVE      *BLANKS       NOTNK2
167900010131     C                   SETOFF                                       35
168000010131     C     TFNT2K        SETLL     TFNTC01L
168100010131     C                   DO        *HIVAL
168200010131     C     TFNT2K        READE     TFNTC01L                               38
168300010131     C   38              LEAVE
168400010131     C     NTCFLT        IFNE      'A'
168500010131     C     TFNT2K        SETLL     TFNTC01L                               35
168600010131     C                   LEAVE
168700010131     C                   ENDIF
168800010131     C                   ENDDO
168900940601E   4C                   ENDIF
169000940601E   3C                   ENDIF
169100030212      *
169200000000      *
169300010131      * SOLO SE E' ANDATO A BUON FINE
169400010131---  C     *IN35         IFEQ      *ON
169500010131     C                   DO        *HIVAL
169600010131     C  N36TFNT2K        READE     TFNTC01L                               35
169700010131     C   36TFNTCK        READE     TFNTC01L                               35
169800010131     C   35              LEAVE
169900010131    4C     NTCSNS        IFEQ      'S'
170000010131     C     NTCFLT        ANDNE     'A'
170100010131    5C     *IN73         IFEQ      '0'
170200010131     C                   SETON                                        7363
170300010131     C                   MOVEL     NTCRNT        DESRS1
170400010131   X5C                   ELSE
170500010131     C                   SETON                                        74
170600050224     C                   MOVEL     NTCRNT        DESRS2
170700010131    5C                   ENDIF
170800010131    4C                   ENDIF
170900010131     C                   ENDDO
171000010131---  C                   ENDIF
171100000000E   2C                   ENDIF
171200940330     C*
171300981113    2C     I45AST        IFEQ      'L'
171400981113     C     I45ACA        ANDNE     *BLANK
171500981113     C                   MOVEL     I45ACA        DESRS1
171600940331     C                   SETON                                        7363
171700940330    2C                   ENDIF
171800000000      *
171900000000E   1C                   ENDIF
172000940331     C* CODICE
172100940331     C  N23STACOD        IFNE      *BLANKS
172200940331     C                   SETON                                        6263
172300940331     C                   ENDIF
172400940331     C*
172500950201     C* DECODIFICO LA NAZIONE SOLO SE DIVERSA DA ITALIA
172600950201     C     WNAR          IFNE      *BLANKS
172700950201     C                   SETON                                        16
172800010626     C                   MOVE      *BLANKS       TBLKEY
172900950201     C                   MOVEL     '15'          TBLCOD
173000950201     C                   MOVEL(P)  WNAR          TBLKEY
173100950201     C     TABELK        CHAIN     TABEL                              34
173200950201     C  N34              MOVEL     TBLUNI        DECNAR           25
173300950201     C   34              CLEAR                   DECNAR
173400950201     C                   ENDIF
173500000000      *
173600000000     C                   ENDSR
173700050908      *************************************************************************
173800050908      * ROUTINE DI stampa spool per testo html
173900050908      *************************************************************************
174000050908     c     prthtml       begsr
174100050908     c                   clear                   pnomn           198
174200050929     c                   clear                   pCosUffCom      198
174300050929     c                   clear                   pNomPo          198
174400050929     c                   clear                   pindpo          198
174500050929     c                   clear                   pCapLocPrvPo    198
174600050929     c                   clear                   pTelPo          198
174700050929     c                   clear                   pFaxPo          198
174800050929     c                   clear                   peman           198
174900050929     c                   clear                   pnomc           198
175000120611
175100120611      * se la filiale è in £6 devo prendere la capofila
175200120611     c                   eval      wpo = keypo
175300120611     c                   exsr      ctrpo
175400120611     c                   eval      keypo = wpo
175500050929
175600050929     c* --------------------------------------
175700050929     c* chaino azorg per reperimento dati p.o.
175800050929     c* --------------------------------------
175900050929     c     keypo         chain     azorg01l
176000051012     C                   MOVEL     ORGDE3        OG143
176100050929     c* ------------------------------------------------------
176200050929     c* Chaino tabella HTML per reperimento e-mail commerciale
176300050929     c* e nominativo commerciale
176400050929     c* ------------------------------------------------------
176500050929     c                   clear                   wemn             60
176600050929     c* se ce l'ho uso il cod.comm
176700050930     c                   if        i45ast <> 'L'
176800060313     c                   if        w§cm1mitt <> *blanks
176900060313     c                   eval      wemn = %trim(w§cm1mitt) +
177000120220     c                             '@BRT.it'
177100050929     c                   else
177200050929     c* se non trovato nemmeno con keypo assumo INFOxxx o INTxxx
177300051012    2c                   if        §ogntw<>'EEX' and §ogntw<>'EUP' and
177400051012     c                             §ogntw<>'DPD' and §ogntw<>'FED'
177500051012     c                   move      orgfil        w003a
177600050930     c                   eval      wemn = 'INFO' + w003a +
177700120220     c                             '@BRT.it'
177800050929     c                   else
177900051012     c* determino ter-part del p.o.
178000051012     c                   clear                   fnlv54ds
178100051012     c                   movel     'P'           i54tpt
178200051012     c                   movel     keypo         i54lin
178300051012     c                   z-add     dateu         i54drf
178400051012     c                   call      'FNLV54R'
178500051012     C                   PARM                    fnlv54ds
178600051012     c                   move      o54tfp        w003a             3
178700050930     c                   eval      wemn = 'INT' + w003a  +
178800120220     c                             '@BRT.it'
178900050929     c                   endif
179000050929     c                   endif
179100050930     c                   else
179200050930     c                   eval      wemn = i45emn
179300050930     c                   end
179400050929     c* --------------------------------------------------------------
179500050929     c* preparo campi per scrittura file di spool per invio testo html
179600050929     c* --------------------------------------------------------------
179700050929     c* commerciale Bartolini
179800050930     c                   if        i45ast <> 'L'
179900051007     c                   eval      pnomn = '§§Firma1§§' + '#' +
180000051109     c                             %trim(§htmnmc) + '}{'
180100050930     c                   else
180200051007     c                   eval      pnomn = '§§Firma1§§' + '#' +
180300051109     c                             %trim(i45nmn) + '}{'
180400050930     c                   endif
180500050929     c* "Ufficio Commerciale"
180600051007     c                   eval      pCosUffCom = '§§Firma2§§' + '#' +
180700051109     c                             'Ufficio Commerciale' + '}{'
180800050929     c* nome po
180900051007     c                   eval      pnompo = '§§Firma3§§' + '#' +
181000070319     c                             'Filiale di:' +
181100051109     c                             %trim(orgdes) + '}{'
181200050929     c* indirizzo p.o.
181300051007     c                   eval      pindpo = '§§Firma4§§' + '#' +
181400051109     c                             %trim(orgind) + '}{'
181500050929     c* cap + loc + provincia p.o.
181600050930     c                   move      orgcpf        w005a             5
181700051007     c                   eval      pCapLocPrvPo = '§§Firma5§§' + '#' +
181800050930     c                             %trim(w005a) +' ' + %trim(orgloc) +' ' +
181900051109     c                             orgpro + '}{'
182000050929     c* telefono p.o.
182100051007     c                   eval      ptelpo = '§§Firma6§§' + '#' +
182200051109     c                             'TEL:' + %trim(orgtel) + '}{'
182300050929     c* FAX      p.o.
182400051007     c                   eval      pfaxpo = '§§Firma7§§' + '#' +
182500051109     c                             'FAX:' + %trim(orgfax) + '}{'
182600050929     c* E-mail commerciale
182700120220     c                   eval      peman = '§§nome@BRT.it§§' + '#' +
182800051109     c                             %trim(wemn) + '}{'
182900050929     c* Nominativo Cliente
183000051007     c                   eval      pnomc = '§§Nominativo§§' + '#' +
183100051109     c                             %trim(w45nmc) + '}{'
183200050929     c*
183300050908     c                   except    rigahtml
183400050908     c                   endsr
183500050908      *************************************************************************
183600050908      * ROUTINE DI stampa errori invio testo html
183700050908      *************************************************************************
183800050908     c     s_errhtml     begsr
183900050909     c* la prima volta che entro eseguo ovrprtf + apertura file di spool
184000050908     c                   if        opnerr = *blanks
184100051003     c                   movel(P)  cmerr(1)      comman
184200050908     c                   z-add     80            lung
184300050908     c                   call      'QCMDEXC'
184400050908     c                   parm                    comman
184500050908     c                   parm                    lung
184600050908     c                   open      fnlv45p
184700050908     c                   seton                                        96
184800050908     c                   movel     '1'           opnerr            1
184900050909     c* imposto campi di stampa testata
185000050909     c                   select
185100050909     c                   when      i45ast='C'
185200050909     c                   eval      p45amb = 'Clienti           '
185300091022     c                   when      i45ast='T'
185400091022     c                   eval      p45amb = 'Trattative        '
185500050909     c                   endsl
185600050909     c                   if        i45ttx <> *blanks
185700050909     c                   seton                                        29
185800050909     c                   clear                   ds1h
185900050909     C                   Z-ADD     1             TBLKUT
186000050909     C                   MOVE      '1H'          TBLCOD
186100050909     C                   MOVEL(p)  i45ttx        TBLKEY
186200050909     C     TABELK        CHAIN     TABEL00F                           99
186300050909     C  N99              MOVEL     TBLUNI        DS1h
186400050909     c                   movel     §1hdes        desttx
186500050909     c                   endif
186600050909     c                   endif
186700050909     c*
186800050909     c   96              write     er_test
186900050908     c                   setoff                                       96
187000050909     c                   select
187100050909     c                   when      i45ast='C'
187200050909     c                   movel(p)  acoksc        p45cod
187300050909     c                   movel(p)  acorag        p45rag
187400091022     c                   when      i45ast='T'
187500110204     c                   movel(p)  visnrv        p45cod
187600091022     c                   movel(p)  acorag        p45rag
187700050909     c                   endsl
187800050909     c                   write     er_riga
187900050908     c                   endsr
188000060313     c* reperimento indirizzo e-mail del commerciale ------------------
188100060313     c     RepEmaMit     begsr
188200060313     c                   clear                   w§cm1mitt        30
188300060313     c* se ce l'ho uso il cod.comm
188400060313     c                   if        i45ast <> 'L'
188500060313     c                   movel     'HTM'         TBECOD
188600060313     c                   if        clpagex > *zeros
188700060313     c                   movel(p)  clpagex       tbeke1
188800060313     c                   movel(p)  'CC'          tbeke2
188900060313     c                   clear                   dhtm
189000060313     c     ktbe          chain     tntbe01l
189100060313     c                   if        %found(tntbe01l) and tbeatb = *blanks
189200060313     c                   movel     tbeuni        dhtm
189300060313     c                   eval      w§cm1mitt = %trim(§htmind)
189400060313     c                   endif
189500060313     c                   endif
189600060313     c* se non trovato cd.comm su htm o cod.comm non presente chaino con p.o
189700060313     c                   if        clpagex = *zeros or  not %found(tntbe01l)
189800060313     c                             or tbeatb <> *blanks
189900060313     c                   movel(p)  keypo         tbeke1
190000060313     c                   movel(p)  'PO'          tbeke2
190100060313     c     ktbe          chain     tntbe01l
190200060313     c                   if        %found(tntbe01l) and tbeatb = *blanks
190300060313     c                   movel     tbeuni        dhtm
190400060313     c                   eval      w§cm1mitt = %trim(§htmind)
190500060313     c                   endif
190600060313     c                   endif
190700060313     c                   endif
190800060313     c                   endsr
190900120611
191000120611      ****************************************************************
191100120611      * Controllo la filiale
191200120611      ****************************************************************
191300120611     c     Ctrpo         BEGSR
191400120611
191500120611     c                   clear                   fnlv55ds
191600120611     c                   eval      d55lin = wpo
191700120611     c                   eval      d55tpt = '6'
191800120611     c                   eval      d55drf = dateu
191900120611     c                   call      'FNLV55R'
192000120611     c                   parm                    fnlv55ds
192100120611     c                   IF        d55tfa <> 0
192200120611     c                   eval      wpo = d55tfa
192300120611     c                   ENDIF
192400120611
192500120611     c                   ENDSR
192600120611
192700000000      *************************************************************************
192800000000      * ROUTINE DI DEFINIZIONI KLIST E VARIABILI
192900000000      *************************************************************************
193000000000     C     DEFVAR        BEGSR
193100000000      *
193200000000     C     TABELK        KLIST
193300000000     C                   KFLD                    TBLKUT
193400000000     C                   KFLD                    TBLCOD
193500000000     C                   KFLD                    TBLKEY
193600050930     C     KTBE          KLIST
193700050930     C                   KFLD                    TBECOD
193800050930     C                   KFLD                    TBEKE1
193900050930     C                   KFLD                    TBEKE2
194000000000     C     CNACOK        KLIST
194100000000     C                   KFLD                    ACOKUT
194200000000     C                   KFLD                    ACOKCC
194300000000     C                   KFLD                    ACOKSC
194400021022     C     KTxt          KLIST
194500021022     C                   KFLD                    $Testo
194600060608     c                   kfld                    $codpo
194700060608     c                   kfld                    $codlin
194800021022     C     KTxt2         KLIST
194900021022     C                   KFLD                    $Testo
195000060608     c                   kfld                    $codpo
195100060608     c                   kfld                    $codlin
195200021022     C                   KFLD                    $TpRig            1
195300000000     C     TFNTCK        KLIST
195400000000     C                   KFLD                    APPL              1
195500000000     C                   KFLD                    NOTNK1           11
195600000000     C                   KFLD                    NOTNK2            4
195700000000     C                   KFLD                    NOTTNT            2
195800000000     C     TFNT2K        KLIST
195900000000     C                   KFLD                    APPLC             1
196000000000     C                   KFLD                    KEY1C            11
196100000000     C                   KFLD                    NOTNK2            4
196200000000     C                   KFLD                    NOTTNT            2
196300041029     C     Kntc          KLIST
196400041029     C                   KFLD                    kapl
196500041029     C                   KFLD                    knk1
196600041029     C                   KFLD                    knk2
196700041029     C                   KFLD                    ktnt
196800970319     C     KACO          KLIST
196900970319     C                   KFLD                    ACOKUT
197000970319     C                   KFLD                    ACOKCC
197100970319     C                   KFLD                    WKSC
197200950201     C*
197300950201     C* DEFINIZIONE CAMPI
197400950201     C     *LIKE         DEFINE    INDCAE        WCAP
197500970319     C     *LIKE         DEFINE    ACOKSC        WKSC
197600000000     C                   ENDSR
197700000000      *************************************************************************
197800000000      *
197900961128     OLET198    E   N21      TESTA            10
198000060608     O                  33   LET01               34
198100060608     O                 N33   LET01               31
198200961119     O          E   N21      TESTA            11
198300960422     O                  33   TD1FIL              29
198400960422     O                 N33   TD1FIL              26
198500961119      * ----  Spett.le  ( 33ON = compresso, 33OFF = normale )
198600961119     O          E   N22      TESTA            12
198700060608     O                  33   LET02               76
198800070301     O                 N33   LET02               54
198900961119     O          E   N21      TESTA            12
199000960422     O                  33   TD1VIA              34
199100960422     O                 N33   TD1VIA              31
199200961119      * ----  Ragione sociale (1° parte se non compresso)
199300961119     O          E   N22      TESTA            13
199400961119     O                  33   WRAG               112
199500961119     O                 N33   RAGIO1              75
199600961119     O          E   N21      TESTA            13
199700960422     O                  33   TD1CAP              14
199800960422     O                  33   TD1LOC              39
199900960422     O                 N33   TD1CAP              11
200000960422     O                 N33   TD1LOC              36
200100961119      * ----  Ragione sociale (2° parte)
200200961119     O          E   N22N33   TESTA            14
200300961119     O                       RAGIO2              61
200400961119     O          E   N21      TESTA            14
200500060608     O                  33   LET03               14
200600060608     O                 N33   LET03               11
200700960422     O                  33   TD2TEL              28
200800960422     O                 N33   TD2TEL              25
200900961119      * ----  indirizzo destinatario
201000961119     O          E   N22      TESTA            15
201100961119     O                  33   INDVIA              96
201200961119     O                 N33   INDVIA              74
201300961119     O          E   N21      TESTA            15
201400060608     O                  33   LET04               14
201500060608     O                 N33   LET04               11
201600960422     O                  33   TD2TFX              28
201700960422     O                 N33   TD2TFX              25
201800960426      * ----  localita' destinatario
201900960426     O          E   N22      TESTA            16
202000960429     O* ESTERO (16ON )  TESTO COMPRESSO (33ON )
202100960429     O                  33 16WCAP                75
202200960429     O                  33 16INDCIT             106
202300960429     O* ESTERO (16ON )  TESTO NORMALE   (33OFF)
202400960426     O                 N33 16WCAP                53
202500960426     O                 N33 16LOCAL3              75
202600960429     O* ITALIA (16OFF)  TESTO NORMALE   (33OFF)
202700960426     O                 N33N16WCAP5               49
202800960426     O                 N33N16LOCAL2              75
202900960429     O* ITALIA (16OFF)  TESTO COMPRESSO (33ON )
203000960429     O                  33N16WCAP                75
203100970306     O                  33N16LOCALP             106
203200060608      * ----  stampa dei riferimenti (61 ON)
203300940331     O          E    61      TESTA            17
203400060608     O                  33   LET05               34
203500060608     O                 N33   LET05               31
203600950201     O* ----  nazione
203700960426     O          E   N22 16   TESTA            17
203800960429     O                  33   WNAR                69
203900960429     O                  33   DECNAR              96
204000960429     O                 N33   WNAR                47
204100960429     O                 N33   DECNAR              74
204200940331     O* ----  codice
204300940331     O          E   N23 62   TESTA       2
204400980803     O                  33                       16 'Cod.Cl.'
204500980803     O                  33   STACOD              28
204600980803     O                 N33                       13 'Cod.Cl.'
204700980803     O                 N33   STACOD              25
204800940314      * ----  Alla c.a.
204900940331     O          E       73   TESTA            21
205000060608     O                  33   LET06               64
205100960426     O                  33   DESRS1             110
205200060608     O                 N33   LET06               42
205300960426     O                 N33   DESRS1              88
205400940314      * ----  e p.c.
205500940331     O          E       74   TESTA       1
205600050224     O                  33   DESRS2             110
205700050224     O                 N33   DESRS2              88
205800940314      * ----  Telefonando chiedere del sig.
205900940331     O          E    63 60   TESTA       2
206000940331     O         OR   N63 60                    21
206100060608     O                  33   LET07               64
206200060608     O                 N33   LET07               61
206300940314      * ----  localita' e data
206400940331     O          E    63N26   TESTA       2
206500940331     O         OR   N63N26 60            2
206600940331     O         OR   N63N26N60                 21
206700960910     O                  33   LOCDAT              44
206800960910     O                 N33   LOCDAT              41
206900940331     O          E            TESTA       2
207000940331     O                                              ' '
207100980227      *-------------------------------------------------------------------------
207200980227      * SE NON VIENE SCELTO NESSUNO DATOP IN TESTATA FACCIO UN SALTO DI 6
207300980227      * RIGHE
207400980227     O          E    21 22 23TESTA       3  3
207500980227     O         AND   24 25 26
207600980227     O         AND   27 28
207700980227     O                                              ' '
207800000000      *-------------------------------------------------------------------------
207900961119     O          E            PAG3             10
208000000000     O                                           63 ' '
208100000000      *-------------------------------------------------------------------------
208200000000     O          E            RIG         1
208300000000     O                       TXTRIG             113
208400000000     O          E            RIG2        0  0
208500000000     O                       TXTRIG             113
208600000000      *-------------------------------------------------------------------------
208700980401     O          E            STCA2            62
208800000000     O                       CAL2               113
208900000000     O          E            STCAR2      0  0
209000000000     O                       CAL2               113
209100000000      *-------------------------------------------------------------------------
209200980401     O          E            STCA1            63
209300000000     O                       CAL1               113
209400000000     O          E            STCAR1      0  0
209500000000     O                       CAL1               113
209600000000      *-------------------------------------------------------------------------
209700971001     O          E   N27      FIRMA       3
209800020626COMP O                  33   RSUTold             90
209900020626NORM O                 N33   RSUTold             64
210000971001     O          E   N27      FIRMA       1
210100060612COMP O                  33   LET08              116
210200060612NORM O                 N33   LET08               90
210300961129     O*
210400961129     O**************************************************************************
210500961129     O**** I N V I O    C O N    F A X *****************************************
210600961129     O**************************************************************************
210700000920     OLET017    E            TESTAF           09
210800970326     O                       STAIMM              51
210900970326     O                                           55 'FAX:'
211000961203     O                       INDTLF              72
211100970326     O                                          106 'Pagina'
211200970326     O                       $PAGIN        Z    110
211300961203     O          E   N21      TESTAF           10
211400060608     O                       LET01               34
211500961129     O          E   N21      TESTAF           11
211600970326     O                       TD1FIL              29
211700961129      * ----  Spett.le  ( 33ON = compresso, 33OFF = normale )
211800961129     O          E   N22      TESTAF           12
211900060608     O                       LET02               76
212000961129     O          E   N21      TESTAF           12
212100970326     O                       TD1VIA              34
212200970326      * ----  Ragione sociale
212300961129     O          E   N22      TESTAF           13
212400970326     O                       WRAG               112
212500961129     O          E   N21      TESTAF           13
212600970326     O                       TD1CAP              14
212700970326     O                       TD1LOC              39
212800961129     O          E   N21      TESTAF           14
212900060608     O                       LET03               14
213000970326     O                       TD2TEL              28
213100961129      * ----  indirizzo destinatario
213200961129     O          E   N22      TESTAF           15
213300970326     O                       INDVIA              96
213400961129     O          E   N21      TESTAF           15
213500060608     O                       LET04               14
213600970326     O                       TD2TFX              28
213700961129      * ----  localita' destinatario
213800961129     O          E   N22      TESTAF           16
213900970326     O* ESTERO (16ON )
214000970326     O                     16WCAP                75
214100970326     O                     16INDCIT             106
214200970326     O* ITALIA (16OFF)
214300970326     O                    N16WCAP                75
214400970326     O                    N16LOCALP             110
214500961129      * ----  stampa dei riferimenti (61 ON)
214600961129     O          E    61      TESTAF           17
214700060608     O                       LET05               34
214800961129     O* ----  nazione
214900961129     O          E   N22 16   TESTAF           17
215000970326     O                       WNAR                69
215100970326     O                       DECNAR              96
215200961129     O* ----  codice
215300961129     O          E   N23 62   TESTAF      2
215400980803     O                                           16 'Cod.'
215500980803     O                       STACOD              28
215600961129      * ----  Alla c.a.
215700961129     O          E       73   TESTAF           21
215800060608     O                       LET06               64
215900970326     O                       DESRS1             110
216000961129      * ----  e p.c.
216100961129     O          E       74   TESTAF      1
216200050224     O                       DESRS2             110
216300961129      * ----  Telefonando chiedere del sig.
216400961129     O          E    63 60   TESTAF      2
216500961129     O         OR   N63 60                    21
216600060608     O                       LET07               64
216700961129      * ----  localita' e data
216800961129     O          E    63N26   TESTAF      2
216900961129     O         OR   N63N26 60            2
217000961129     O         OR   N63N26N60                 21
217100970326     O                       LOCDAT              44
217200961129     O          E            TESTAF      2
217300961129     O                                              ' '
217400961129      *-------------------------------------------------------------------------
217500970318     O          E            PAG2             09
217600970326     O                                          106 'Pagina'
217700970326     O                       $PAGIN        Z    110
217800961129     O          E            PAG3F            10
217900961129     O                                           63 ' '
218000961129      *-------------------------------------------------------------------------
218100961129     O          E            RIGF        1
218200961129     O                       TXTRIG             113
218300961129     O          E            RIG2F       0  0
218400961129     O                       TXTRIG             113
218500961129      *-------------------------------------------------------------------------
218600961129     O          E            STCA2F           64
218700961129     O                       CAL2               113
218800961129     O          E            STCR2F      0  0
218900961129     O                       CAL2               113
219000961129      *-------------------------------------------------------------------------
219100961129     O          E            STCA1F           65
219200961129     O                       CAL1               113
219300961129     O          E            STCR1F      0  0
219400961129     O                       CAL1               113
219500961129      *-------------------------------------------------------------------------
219600971001     O          E   N27      FIRMAF      3
219700020626COMP O                       RSUTold             90
219800971001     O          E   N27      FIRMAF      1
219900060612COMP O                       LET08              116
220000961129     O*
220100970326     O**************************************************************************
220200970326     OLETNOR    E            TESTAN           09
220300970326     O                       STAIMM              51
220400970326     O                                           55 'FAX:'
220500970326     O                       INDTLF              72
220600970326     O                                           79 'Pagina'
220700970326     O                       $PAGIN        Z     83
220800970326     O          E   N21      TESTAN           10
220900060608     O                       LET01               31
221000970326     O          E   N21      TESTAN           11
221100970326     O                       TD1FIL              26
221200970326     O          E   N22      TESTAN           12
221300060608     O                       LET02               54
221400970326     O          E   N21      TESTAN           12
221500970326     O                       TD1VIA              31
221600970326      * ----  Ragione sociale (1° PARTE)
221700970326     O          E   N22      TESTAN           13
221800970326     O                       RAGIO1              75
221900970326     O          E   N21      TESTAN           13
222000970326     O                       TD1CAP              11
222100970326     O                       TD1LOC              36
222200970326      * ----  Ragione sociale (2° parte)
222300970326     O          E   N22      TESTAN           14
222400970326     O                       RAGIO2              61
222500970326     O          E   N21      TESTAN           14
222600060608     O                       LET03               11
222700970326     O                       TD2TEL              25
222800970326      * ----  indirizzo destinatario
222900970326     O          E   N22      TESTAN           15
223000970326     O                       INDVIA              74
223100970326     O          E   N21      TESTAN           15
223200060608     O                       LET04               11
223300970326     O                       TD2TFX              25
223400970326      * ----  localita' destinatario
223500970326     O          E   N22      TESTAN           16
223600970326     O* ESTERO (16ON )  TESTO NORMALE
223700970326     O                     16WCAP                53
223800970326     O                     16LOCAL3              75
223900970326     O* ITALIA (16OFF)  TESTO NORMALE
224000970326     O                    N16WCAP5               49
224100970326     O                    N16LOCAL2              75
224200970326      * ----  stampa dei riferimenti (61 ON)
224300970326     O          E    61      TESTAN           17
224400060608     O                       LET05               31
224500970326     O* ----  nazione
224600970326     O          E   N22 16   TESTAN           17
224700970326     O                       WNAR                47
224800970326     O                       DECNAR              74
224900970326     O* ----  codice
225000970326     O          E   N23 62   TESTAN      2
225100970326     O                                           10 'Cod.'
225200970326     O                       STACOD              22
225300970326      * ----  Alla c.a.
225400970326     O          E       73   TESTAN           21
225500060608     O                       LET06               42
225600970326     O                       DESRS1              88
225700970326      * ----  e p.c.
225800970326     O          E       74   TESTAN      1
225900050224     O                       DESRS2              88
226000970326      * ----  Telefonando chiedere del sig.
226100970326     O          E    63 60   TESTAN      2
226200970326     O         OR   N63 60                    21
226300060608     O                       LET07               61
226400970326      * ----  localita' e data
226500970326     O          E    63N26   TESTAN      2
226600970326     O         OR   N63N26 60            2
226700970326     O         OR   N63N26N60                 21
226800970326     O                       LOCDAT              41
226900970326     O          E            TESTAN      2
227000970326     O                                              ' '
227100970326      *-------------------------------------------------------------------------
227200970326     O          E            PAG2N            09
227300970326     O                                           79 'Pagina'
227400970326     O                       $PAGIN        Z     83
227500970326     O          E            PAG3N            10
227600970326     O                                           63 ' '
227700970326      *-------------------------------------------------------------------------
227800970326     O          E            RIGN        1
227900970326     O                       TXTRIG             113
228000970326     O          E            RIG2N       0  0
228100970326     O                       TXTRIG             113
228200970326      *-------------------------------------------------------------------------
228300970326     O          E            STCA2N           64
228400970326     O                       CAL2               113
228500970326     O          E            STCR2N      0  0
228600970326     O                       CAL2               113
228700970326      *-------------------------------------------------------------------------
228800970326     O          E            STCA1N           65
228900970326     O                       CAL1               113
229000970326     O          E            STCR1N      0  0
229100970326     O                       CAL1               113
229200970326      *-------------------------------------------------------------------------
229300971001     O          E   N27      FIRMAN      3
229400020626NORM O                       RSUTold             64
229500971001     O          E   N27      FIRMAN      1
229600060612NORM O                       LET08               90
229700041029     O**************************************************************************
229800041029     O**** I N V I O    C O N    E-MAIL ****************************************
229900041029     O**************************************************************************
230000041104     Olet017m   E   N21      TESTAFM          02
230100060608     O                       LET01               34
230200041104     O          E   N21      TESTAFM          03
230300041029     O                       TD1FIL              29
230400041029      * ----  Spett.le  ( 33ON = compresso, 33OFF = normale )
230500041104     O          E   N22      TESTAFM          04
230600060608     O                       LET02               76
230700041104     O          E   N21      TESTAFM          04
230800041029     O                       TD1VIA              34
230900041029      * ----  Ragione sociale
231000041104     O          E   N22      TESTAFM          05
231100041029     O                       WRAG               112
231200041104     O          E   N21      TESTAFM          05
231300041029     O                       TD1CAP              14
231400041029     O                       TD1LOC              39
231500041104     O          E   N21      TESTAFM          06
231600060608     O                       LET03               14
231700041029     O                       TD2TEL              28
231800041029      * ----  indirizzo destinatario
231900041104     O          E   N22      TESTAFM          07
232000041029     O                       INDVIA              96
232100041104     O          E   N21      TESTAFM          07
232200060608     O                       LET04               14
232300041029     O                       TD2TFX              28
232400041029      * ----  localita' destinatario
232500041104     O          E   N22      TESTAFM          08
232600041029     O* ESTERO (16ON )
232700041029     O                     16WCAP                75
232800041029     O                     16INDCIT             106
232900041029     O* ITALIA (16OFF)
233000041029     O                    N16WCAP                75
233100041029     O                    N16LOCALP             110
233200041029      * ----  stampa dei riferimenti (61 ON)
233300041104     O          E    61      TESTAFM          09
233400060608     O                       LET05               34
233500041029     O* ----  nazione
233600041104     O          E   N22 16   TESTAFM          09
233700041029     O                       WNAR                69
233800041029     O                       DECNAR              96
233900041029     O* ----  codice
234000041029     O          E   N23 62   TESTAFM     2
234100041029     O                                           16 'Cod.'
234200041029     O                       STACOD              28
234300041029      * ----  Alla c.a.
234400041104     O          E       73   TESTAFM          13
234500060608     O                       LET06               64
234600041029     O                       DESRS1             110
234700041029      * ----  e p.c.
234800041029     O          E       74   TESTAFM     1
234900050224     O                       DESRS2             110
235000041029      * ----  Telefonando chiedere del sig.
235100041029     O          E    63 60   TESTAFM     2
235200041104     O         OR   N63 60                    13
235300060608     O                       LET07               64
235400041029      * ----  localita' e data
235500041029     O          E    63N26   TESTAFM     2
235600041029     O         OR   N63N26 60            2
235700041104     O         OR   N63N26N60                 13
235800041029     O                       LOCDAT              44
235900041029     O          E            TESTAFM     2
236000041029     O                                              ' '
236100041029      *-------------------------------------------------------------------------
236200041029     O          E            RIGFM       1
236300041029     O                       TXTRIG             113
236400041029     O          E            RIG2FM      0  0
236500041029     O                       TXTRIG             113
236600041029      *-------------------------------------------------------------------------
236700041126     O          E            STCA2FM     2  0
236800041029     O                       CAL2               113
236900041029     O          E            STCR2FM     0  0
237000041029     O                       CAL2               113
237100041126      *-------------------------------------------------------------------------
237200041126     O          E            VUO1FM      1  0
237300041126      *-------------------------------------------------------------------------
237400041126     O          E            VUO2FM      2  0
237500041029      *-------------------------------------------------------------------------
237600041126     O          E            STCA1FM     0  0
237700041029     O                       CAL1               113
237800041029     O          E            STCR1FM     0  0
237900041029     O                       CAL1               113
238000041029      *-------------------------------------------------------------------------
238100041029     O          E   N27      FIRMAFM     3
238200041029COMP O                       RSUTold             90
238300041029     O          E   N27      FIRMAFM     1
238400060612COMP O                       LET08              116
238500041029     O*
238600041029     O**************************************************************************
238700041104     Oletnorm   E   N21      TESTANM          02
238800060608     O                       LET01               31
238900041104     O          E   N21      TESTANM          03
239000041029     O                       TD1FIL              26
239100041104     O          E   N22      TESTANM          04
239200060608     O                       LET02               54
239300041104     O          E   N21      TESTANM          04
239400041029     O                       TD1VIA              31
239500041029      * ----  Ragione sociale (1° PARTE)
239600041104     O          E   N22      TESTANM          05
239700041029     O                       RAGIO1              75
239800041104     O          E   N21      TESTANM          05
239900041029     O                       TD1CAP              11
240000041029     O                       TD1LOC              36
240100041029      * ----  Ragione sociale (2° parte)
240200041104     O          E   N22      TESTANM          06
240300041029     O                       RAGIO2              61
240400041104     O          E   N21      TESTANM          06
240500060608     O                       LET03               11
240600041029     O                       TD2TEL              25
240700041029      * ----  indirizzo destinatario
240800041104     O          E   N22      TESTANM          07
240900041029     O                       INDVIA              74
241000041104     O          E   N21      TESTANM          07
241100060608     O                       LET04               11
241200041029     O                       TD2TFX              25
241300041029      * ----  localita' destinatario
241400041104     O          E   N22      TESTANM          08
241500041029     O* ESTERO (16ON )  TESTO NORMALE
241600041029     O                     16WCAP                53
241700041029     O                     16LOCAL3              75
241800041029     O* ITALIA (16OFF)  TESTO NORMALE
241900041029     O                    N16WCAP5               49
242000041029     O                    N16LOCAL2              75
242100041029      * ----  stampa dei riferimenti (61 ON)
242200041104     O          E    61      TESTANM          09
242300060608     O                       LET05               31
242400041029     O* ----  nazione
242500041104     O          E   N22 16   TESTANM          09
242600041029     O                       WNAR                47
242700041029     O                       DECNAR              74
242800041029     O* ----  codice
242900041029     O          E   N23 62   TESTANM     2
243000041029     O                                           10 'Cod.'
243100041029     O                       STACOD              22
243200041029      * ----  Alla c.a.
243300041104     O          E       73   TESTANM          13
243400060608     O                       LET06               42
243500041029     O                       DESRS1              88
243600041029      * ----  e p.c.
243700041029     O          E       74   TESTANM     1
243800050224     O                       DESRS2              88
243900041029      * ----  Telefonando chiedere del sig.
244000041029     O          E    63 60   TESTANM     2
244100041104     O         OR   N63 60                    13
244200060608     O                       LET07               61
244300041029      * ----  localita' e data
244400041029     O          E    63N26   TESTANM     2
244500041029     O         OR   N63N26 60            2
244600041104     O         OR   N63N26N60                 13
244700041029     O                       LOCDAT              41
244800041029     O          E            TESTANM     2
244900041029     O                                              ' '
245000041029      *-------------------------------------------------------------------------
245100041029     O          E            RIGNM       1
245200041029     O                       TXTRIG             113
245300041029     O          E            RIG2NM      0  0
245400041029     O                       TXTRIG             113
245500041029      *-------------------------------------------------------------------------
245600041126     O          E            STCA2NM     2  0
245700041029     O                       CAL2               113
245800041029     O          E            STCR2NM     0  0
245900041029     O                       CAL2               113
246000041126      *-------------------------------------------------------------------------
246100041126     O          E            VUO1NM      1  0
246200041126      *-------------------------------------------------------------------------
246300041126     O          E            VUO2NM      2  0
246400041029      *-------------------------------------------------------------------------
246500041126     O          E            STCA1NM     0  0
246600041029     O                       CAL1               113
246700041029     O          E            STCR1NM     0  0
246800041029     O                       CAL1               113
246900041029      *-------------------------------------------------------------------------
247000041029     O          E   N27      FIRMANM     3
247100041029NORM O                       RSUTold             64
247200041029     O          E   N27      FIRMANm     1
247300060612NORM O                       LET08               90
247400050908     O**************************************************************************
247500050908     O**** I N V I O    C O N    E-MAIL XTML ***********************************
247600050908     O**************************************************************************
247700051007     Olethtml   E            rigahtml       1
247800051007     o                       pnomc
247900051007     O          E            rigahtml       1
248000050929     o                       pnomn
248100050908     O          E            rigahtml       1
248200050929     o                       pCosUffCom
248300050908     O          E            rigahtml       1
248400050929     o                       pNomPo
248500050929     O          E            rigahtml       1
248600050929     o                       pIndPo
248700050929     O          E            rigahtml       1
248800050929     o                       pCapLocPrvPo
248900050929     O          E            rigahtml       1
249000050929     o                       pTelPo
249100050929     O          E            rigahtml       1
249200050929     o                       pFaxPo
249300050929     O          E            rigahtml       1
249400050929     o                       peman
249500000000      *************************************************************************
249600000000** Comandi    567890123456789012345678901234567890123456789012345678901234567890
249700000920OVRPRTF FILE(LET198) CPI(15)   COPIES(001)  USRDTA('Lettere') FORMTYPE(LET1)
249800960910** SDT
249900960910, li XX/XX/XXXX
250000961129**
250100000920OVRPRTF LET017  LET198 CPI(15)   OUTQ(          ) FORMTYPE(LET1)      1
250200000920  COPIES(001) USRDTA('LetFax')                                        2
250300041102**
250400041102DLTOVR FILE(       )
250500041102**
250600041102OVRPRTF LET017M LET198 CPI(15)   OUTQ(          ) FORMTYPE(LET1)      1
250700041102  COPIES(001) USRDTA('LetMail')  usrdfndta('                          2
250800050908**
250900050908OVRPRTF LETHTML LET198 CPI(15)   OUTQ(          ) FORMTYPE(LET1)      1
251000050908  COPIES(001) USRDTA('LetHtml')  usrdfndta('                          2
251100050908**
251200050908OVRPRTF FNLV45P USRDTA('ErrHtml')                                     1
