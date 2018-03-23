000100950523     H DECEDIT('0,') DATEDIT(*DMY.)
000200101216     h dftactgrp(*no) actgrp(*caller)
000300101209     H* Fnlrq1R *-----------------------------------------------------*
000400101209     H*            sistema DISTINTE CONSEGNA
000500930225     H*---------------------------------------------------------------*
000600101209     FFnlrq1D   CF   E             WORKSTN
000800110210     Ffidst04l  IF   E           K DISK
000801110210     Ffiapd01L  IF   E           K DISK
000802110202     Ftntbe01L  IF   E           K DISK
001000110103     D fidstds       E DS                  extname(fidst00f)
001001110209     D fnlr48ds      E DS
001002110209     D fnlrq3ds      E DS
001003110209     D fnlrf8ds      E DS
001004110103     D fidg37ds      E DS
001005110202     D dvpocont      E DS
001006110202      * DS per richiamo pgm FNLR36R interrogazione bolle
001007110202     D DSLR36          DS
001008110202     D  P36AAS                14     17  0
001009110202     D  P36LNP                18     20  0
001010110202     D  P36NRS                21     22  0
001011110202     D  P36NSP                23     29  0
001012110202     D  P36F03                30     30
001013110202     D  P36FLG                31     31
001014110103      * DS  pgm fidg44r x forzatura distinta con fase CUS                   ti
001015110103     D fidg44ds      E DS
001100050519     D* DS PER APDFLR  - filler anagrafica  autotrasp.
001200050519     D dapdflr       E DS
001500050519     D* DS PER FNLV24R - ricerca autotrasp.
001600050519     D fnlv24ds      E DS
001700021203     D* DS PER FNLV50R - CONTROLLO FILIALE GESTIONE
001800021203     D DSLV50        E DS                  EXTNAME(FNLV50DS)
001900950523     D* DS PER FNLR02R - INTERROGAZIONE FOGLI VARI
002000950523     D DSLR01        E DS
002100101209     d*
002200101209     D KPJBA         E DS
002300101209     d* Parametri x Controllo profilo utenti
002400101209     d TIBS34ds      e ds
002500101209     d AZUTEds       e ds                  extname(AZUTE00F)
002600101209     d* Ds per dati organigramma
002700101209     d DDatiUte      e ds
002701110209     d*
002702110209     d DSARBD        e ds
002703110209     d DSARBK        e ds
002704110209     d DARBT         e ds
002705110209     d DSARBG        e ds
002706110209     d trul90ds      e ds
002900101216RDE  D* DS PER FNLR59R - quadratura DISTINTA
003000101216RDE  D FNLR59ds      E DS
003001101220RDE  D FNLRq2ds      E DS
003100950524     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
003200950524     D WLBDAT          DS                  INZ
003300950524     D  G02DAT                 1      8  0
003400950524     D  G02INV                 9     16  0
003500950524     D  G02ERR                17     17
003600950524     D  G02TGI                18     22  0
003700950523     D                 DS
003800950523     D  V1CFPD                 1      3  0
003900950523     D  V1CCPC                 4      7
004000950523     D  WPDR                   1      7
004100101209     D* DEFINIZIONE SCHIERE
004200101209     D MSG             S             78    DIM(07) CTDATA PERRCD(1)             MESSAGGI ERRORE
004300061002     C**********************************************************************
004400110210     C                   CLEAR                   V1Cchk
004401110210     C                   CLEAR                   V1CNFV
004500950525     C                   CLEAR                   V1CCPC
004501110104     C                   CLEAR                   fnlrq2ds
004600950524     C* EMISSIONE FORMATO
004700060929     C                   do        *hival
004701110104     c* ricarica il sfl degli errori senza riemettre la richiesta della
004702110104     c* distinta
004703110104     c                   if        lrq2cmdo <> '05'
004800950523     C                   EXFMT     LRF0D01
004900950523     C                   CLEAR                   V1CMSG
005000950523     C                   SETOFF                                       2840
005100950525     C                   SETOFF                                       414243
005200101209     C   kc              leave
005201110211     c** serve per il posizionamento sulla distinta o sul barcode
005202110211     C                   if        *inkb
005203110211     c                   if        *in36
005205110211     c                   eval      v03f2 = 'F2=Pistola'
005207110211     c                   setoff                                         36
005208110211     c                   else
005210110211     c                   eval      v03f2 = 'F2=Manuale'
005211110211     c                   seton                                            36
005212110211     c                   end
005213110211     c                   iter
005214110211     C                   END
005301110209      *  manutenzione bolle
005302110209     c                   If        *inKK  = '1'
005303110209     c                   clear                   dslr36
005304110209     C                   MOVEL     'M'           P36FLG
005305110209     C                   MOVEL     DSLR36        KPJBU
005306110209     C                   CALL      'FNLR36R'
005307110209     C                   PARM                    KPJBA
005324110209     c                   iter
005330110202     c                   end
005331110209      *  Interrogazione  bolle
005332110209     c                   If        *inKJ  = '1'
005333110209     c                   clear                   dslr36
005334110209     C                   MOVEL     '2'           P36FLG
005335110209     C                   MOVEL     DSLR36        KPJBU
005336110209     C                   CALL      'FNLR36R'
005337110209     C                   PARM                    KPJBA
005338110209     c                   iter
005339110209     c                   end
005350110202     C** CMD18 - CAMBIO FILIALE
005400950526    1C     *INKS         IFEQ      *ON
005500950526     C                   SETON                                        04
005600060929     C                   iter
005700950526    1C                   ENDIF
005800950523     C* 04 ON  - CONTROLLO FILIALE IN GESTIONE
005900950524    1C     *IN04         IFEQ      *ON
006000110103     C                   MOVEL     V1CFGS        WFIL              3 0
006100950523     C                   EXSR      CTRFGS
006200060929     C   28              iter
006300950524    1C                   ENDIF
006301110202     C* controllo se po partito
006302110202     c                   eval      tbecod = 'VPO'
006303110202     c                   eval      tbeke1 = 'CONT'
006304110202     c                   movel(p)  v1cfgs        tbeke2
006305110202     c     ktbe2         chain     tntbe01l
006306110202     c                   if        not %found(tntbe01l)
006307110202     c     ktbe1         chain     tntbe01l
006308110202     c                   if        not %found(tntbe01l)
006309110202     c                   seton                                        lr
006310110202     c                   return
006311110202     c                   else
006312110202     c                   movel(p)  tbeuni        dvpocont
006313110202     c                   if        §vposqa > dateu8
006314110202     c                   seton                                        lr
006315110202     c                   return
006316110202     c                   end
006317110202     c                   end
006318110202     c                   end
006400950523     C*
006500950523     C* CONTROLLI VIDEATA
006600950523     C                   EXSR      CTRD01
006700950524     C   28
006800060929     COR 90              iter
006801110104     c                   end
006802110202     C** stampa controllo aut
006803110202    1C     *INKG         IFEQ      *ON
006804110202     C                   exsr      sbmp60
006805110202     c                   commit
006806110202     c                   iter
006807110202    1C                   ENDIF
006808110209      *  Bancali
006809110209     c                   If        *inKh  = '1'
006810110209     c                   clear                   fnlrf8ds
006811110209     c                   move      'E'           rf8mod
006812110209     c                   z-add     v1cfgs        rf8fgs
006813110209     c                   move      v1cnfv        rf8dis
006814110209     C                   movel     fnlrf8ds      KPJBU
006815110209     C                   CALL      'FNLRF8R'
006816110209     C                   PARM                    KPJBA
006817110209     c                   iter
006818110209     c                   end
006819110202     c*
006900101209     c                   exsr      srges01
006901110104     c                   select
006902101220     c* comando fine
006903110104     c                   when      lrq2cmdo = '03'
006904101220     c                   leave
007002101220     c* se errore
007003110104     c                   when      lrq2msgo <> ' '
007004101220     c                   seton                                        28
007005110103     c                   eval      v1cmsg = lrq2msgo
007007110104     c* rivisualizza
007008110104     c                   when      lrq2cmdo = '05'
007011110104     c* se tutto ok
007012110104     c                   when      lrq2msgo = ' '
007013110104     c                   exsr      srinz
007014110104     c                   endsl
007100060929     c                   enddo
007200101216     c*
007201110202     c                   commit
007300101216     C                   SETON                                        LR
007400060929     C**********************************************************************
007500101216     C*-- pulisco i dati a video
007600060929     C**********************************************************************
007700101216     C     srinz         BEGSR
007800110210     C                   CLEAR                   V1Cchk
007801110210     C                   CLEAR                   V1CNFV
007900101216     C                   CLEAR                   V1CCPC
008000101216     C                   CLEAR                   V1CMSG
008100101216     C                   SETOFF                                       2840
008200101216     C                   SETOFF                                       414243
008201110104     c                   clear                   fnlrq2ds
008300101216     c                   endsr
008400101216     C**********************************************************************
008500101216     C*-- richiamo gestione errori
008600101216     C**********************************************************************
008700101216     C     srges01       BEGSR
008800101216     c*
008900101216RDE  C                   CLEAR                   FNLR59ds
008901101220RDE  C                   CLEAR                   FNLRq2ds
009000101216     C                   eval      d59lsr = 'S'
009100101216RDE  C                   MOVE      V1CFGS        D59fgs
009200101216RDE  C                   MOVE      V1CNFV        D59NDD
009300101216RDE  C                   MOVE      V1CNFV        D59NDA
009400110103RDE  C                   MOVE      dstdfv        D59ddt
009500101216RDE  C                   MOVEL(p)  V1dPDR        D59dsf
009600101216RDE  C                   MOVE      wpdr          D59pdr
009700110103RDE  C                   MOVEl     dsttpm        D59tpm
009800101216RDE  C                   clear                   D59tpd
009900110103RDE  C                   MOVEL     dstFPP        D59FPP
009901160429     C* imposto primo e secondo turno di dft per non controllare il turno
009902160429     c* nella selezione della distinta
009903160429RDE  C                   MOVEL     '1'           D59tur1
009904160429RDE  C                   MOVEL     '2'           D59tur2
010000101216     C                   MOVEL     'A'           d59STP
010100101216     C                   MOVEL     'N'           d59SOR
010200101216     C                   MOVEL     'S'           d59SQA
010300101216RDE  C                   MOVEL(p)  FNLR59ds      KPJBU
010400101216     c                   call      'FNLRQ2C'
010500101216     c                   parm                    kpjba
010501101220     c                   parm                    fnlrq2ds
010507110127     c* per Conferma richiamo il ricalcolare dei saldi
010508110127     c                   if        lrq2cmdo = '06' or
010509110127     c                             lrq2cmdo = '07'
010510110103     c                   exsr      srfidg37
010511110126     c                   end
010512110127     c* confermo le variazioni
010513110127     c                   commit
010514110127      *  per Conferma + stampa
010515110127     c                   if        lrq2cmdo = '07'
010516110127     c                   exsr      sbmp60
010517110127     c                   end
010600101216     c*
010700101216     c                   endsr
010701110103     C*-----------------------------------------------------*
010702110103     C* aggiorna pesi/volumi/fermate assegnate x fidst00f   *
010703110103     C*-----------------------------------------------------*
010704110103     C     srfidg37      BEGSR
010705110103     c*
010707110103     c                   clear                   fidg37ds
010708110103     c                   eval      D37FGSI = dstfgs
010709110103     c                   eval      D37DFVI = dstdfv
010710110103     c                   eval      D37NFVI = dstnfv
010711110103     c                   eval      D37cmti = 'N'
010712110103     c                   eval      kpjbu = fidg37ds
010713110103     c                   call      'FIDG37R'
010714110103     c                   parm                    kpjba
010715110103     c                   eval      fidg37ds = kpjbu
010722110103     c*
010723110103     C                   ENDSR
010724110127     C*--------------------------------------------------------------*
010725110127     C*  Sottometto controllo aut. in stampa
010726110127     C*--------------------------------------------------------------*
010727110127     C     SBMP60        BEGSR
010728110127     C*
010729110202RDE  C                   CLEAR                   FNLR59ds
010731110202     C                   eval      d59lsr = 'S'
010732110202RDE  C                   MOVE      V1CFGS        D59fgs
010733110202RDE  C                   MOVE      V1CNFV        D59NDD
010734110202RDE  C                   MOVE      V1CNFV        D59NDA
010735110202RDE  C                   MOVE      dstdfv        D59ddt
010736110202RDE  C                   MOVEL(p)  V1dPDR        D59dsf
010737110202RDE  C                   MOVE      wpdr          D59pdr
010738110202RDE  C                   MOVEl     dsttpm        D59tpm
010739110202RDE  C                   clear                   D59tpd
010740110202RDE  C                   MOVEL     dstFPP        D59FPP
010741110202     C                   MOVEL     'A'           d59STP
010742110202     C                   MOVEL     'N'           d59SOR
010743110202     C                   eval      d59sqa = ' '
010744110202RDE  C                   MOVEL(p)  FNLR59ds      KPJBU
010749110127     C                   CALL      'FNLRF7R5'
010750110127     C                   PARM                    KPJBA
010751110127     C*
010752110127     C                   ENDSR
010800101216     C**********************************************************************
010900101216     C*--- CONTROLLO LA FILIALE IN GESTIONE --------------------------*
011000101216     C**********************************************************************
011100101216     C     CTRFGS        BEGSR
011200950523     C                   CLEAR                   DSLV50
011300950523     C                   MOVEL     KNMUS         D50PRU
011400950523     C                   MOVEL     WFIL          D50FGS
011500950523     C                   CALL      'FNLV50R'
011600950523     C                   PARM                    DSLV50
011700950523     C*
011800950524    1C     D50ERR        IFNE      ' '
011900950523     C                   MOVEL     D50MSG        V1CMSG
012000950525     C                   SETON                                        4328
012100950524    1C                   ENDIF
012200950523     C*
012300060929     C                   ENDSR
012400060929     C**********************************************************************
012500950523     C*--- CONTROLLO DATI VIDEATA ------------------------------------*
012600060929     C**********************************************************************
012700950523     C     CTRD01        BEGSR
012800101209     C                   SETOFF                                         90
012801110211     c                   clear                   wpadr
013000950607     C****  CODICE PADRONCINO  ****
013100950607     C* RICERCA
013200950607     C     '?'           SCAN      V1CCPC                                 90
013300950607    1C     *IN90         IFEQ      *ON
013400021203     c                   clear                   fnlv24ds
013500021203     C                   Z-ADD     V1CFGS        d24FIL
013600021203     C                   MOVEL     'R'           d24FLG
013700021203     C                   MOVEL     'A'           d24tip
013800021203     C                   MOVEL(p)  fnlv24ds      KPJBU
013900950607     C                   CALL      'FNLV24R'
014000950607     C                   PARM                    KPJBA
014100021203     C                   MOVEL     KPJBU         fnlv24ds
014200950607     C* CONTROLLO SE E' STATO SELEZIONATO UN CODICE PADRONCINO
014300021203     C     d24pdr        IFNE      0
014400021203     C                   MOVEL     d24pdr        WPDR
014500101216     C                   MOVEL     d24rsc        V1DPDR
014501110210     c* propongo l'ultima distinta dell'aut
014502110210     C                   eval      WPaDR = d24pdr
014503110210     c                   clear                   dstfcf
014504110210     c     kdst4         setgt     fidst04l
014505110210     c     kdst4         readpe    fidst04l
014506110210     c                   if        not %eof(fidst04l)
014507110210     c                   move      dstnfv        v1cnfv
014508110210     c                   else
014509110210     c                   clear                   v1cnfv
014510110210     c                   end
014600950607     C                   ELSE
014700950607     C                   CLEAR                   V1CCPC
014800950607     C                   ENDIF
014801110210     C                   leavesr
015000950607    1C                   ENDIF
015100950607     C* CONTROLLO VALIDITA' DEL CODICE PADRONCINO
015200950607    1C     V1CCPC        IFNE      *BLANKS
015300950607     C     V1CCPC        ANDNE     *ZEROS
015400950607     C                   TESTN                   V1CCPC               35
015500950607    2C     *IN35         IFEQ      *ON
015600950607     C                   MOVE      V1CCPC        W001              1
015700950607     C* CONTROLLO SE L'ULTIMO CARATTERE E' UNA LETTERA
015800950607    3C     W001          IFLT      '0'
015900950607     C                   SETOFF                                       35
016000950607    3C                   ENDIF
016100950607    2C                   ENDIF
016200950607    2C     *IN35         IFEQ      *OFF
016300101209     C                   MOVEL     MSG(06)       V1CMSG
016400950607     C                   SETON                                        4128
016500110210     C                   leavesr
016600950607    2C                   ENDIF
016700950607     C* CONTROLLO SE ESISTE NELL'ANAGRAFICA PADRONCINI
016800950607     C                   MOVE(P)   WPDR          WPADR
016900061002     C     kapdw         CHAIN     fiapd01l
017000061002     c                   if        not %found(fiapd01l) or
017100061002     c                             (%found(fiapd01l) and apdatb <> ' ')
017200101209     C                   MOVEL     MSG(06)       V1CMSG
017300950607     C                   SETON                                        4128
017400110210     C                   leavesr
017500070123     c                   else
017600070123     c                   eval      dapdflr = apdflr
017700950607    1C                   ENDIF
017800061003    1C                   ENDIF
017900110103     C****  NUMERO DISTINTA  ****
018000110103     C* RICERCA
018100950523     C     '?'           SCAN      V1CNFV                                 90
018200950523    1C     *IN90         IFEQ      *ON
018300950523     C                   CLEAR                   DSLR01
018400950523     C                   Z-ADD     DATEU8        DLRADA
018500950523     C                   Z-ADD     V1CFGS        DLRFGS
018600110324     C                   MOVEL     'FNLRQ1R'     DLRPGM
018700950523     C                   MOVEL     'S'           DLRRIC
018800950523     C                   MOVEL     '2'           DLRTFV
018900950523     C                   Z-ADD     4             DLRNPG
019000950523     C                   MOVEL     DSLR01        KPJBU
019100070223     C                   CALL      'FNLR02R'
019200061228     C                   PARM                    KPJBA
019300950523     C                   MOVEL     KPJBU         DSLR01
019400070627     c                   movel     *all'0'       v1cnfv
019500070627     C                   move      DLRNFV        v1cnfv
019501110210     C                   leavesr
019700950523    1C                   ENDIF
019800110211     C* se gestione manuale
019801110211     c                   if        not *in36
019802110211     c                   clear                   v1cchk
019900950523    1C     V1CNFV        IFNE      *BLANKS
020000950523     C     V1CNFV        ANDNE     *ZEROS
020100950523     C                   TESTN                   V1CNFV               35
020200950523    2C     *IN35         IFEQ      *ON
020300950523     C                   MOVE      V1CNFV        W001
020400950523     C* CONTROLLO SE L'ULTIMO CARATTERE E' UNA LETTERA
020500950523    3C     W001          IFLT      '0'
020600950523     C                   SETOFF                                       35
020700950523    3C                   ENDIF
020800950523    2C                   ENDIF
020900950523    2C     *IN35         IFEQ      *OFF
021000950524     C                   MOVEL     MSG(1)        V1CMSG
021100950524     C                   SETON                                        4028
021200110210     C                   leavesr
021300950523    2C                   ENDIF
021301110211    2C                   ENDIF
024000110211   X3C                   Else
024001110211     C* se gestione pistola  (movel perchè ultimo byte check digit)
024002110211     c                   movel     v1cchk        v1cnfv
024003110211     c                   end
024004110210     c* propongo l'ultima distinta dell'aut
024005110211     c                   if        v1cnfv = ' ' or v1cnfv =*zeros
024101110210     c                   clear                   dstfcf
024102110211     C                   MOVE(P)   WPDR          WPADR
024103110210     c     kdst4         setgt     fidst04l
024104110210     c     kdst4         readpe    fidst04l
024105110210     c                   if        not %eof(fidst04l)
024106110210     c                   move      dstnfv        v1cnfv
024107110210     c                   else
024108110210     C* NUMERO DISTINTA INESISTENTE
024200110202     C                   MOVEL     MSG(1)        V1CMSG
024300950524     C                   SETON                                        4028
024400110210     C                   leavesr
024500950524    3C                   ENDIF
024501110210    3C                   ENDIF
024502110103     c* aggancio la distinta e faccio tutti i controlli
024503110103     c                   clear                   fnlrq3ds
024504110103     c                   clear                   fidstds
024505110103     c                   z-add     4             lrq3npgi
024506110103     c                   z-add     v1cfgs        lrq3fgsi
024507110103     c                   move      v1cnfv        lrq3ndci
024508110103     c                   eval      lrq3alci = 'S'
024509110103     c                   movel     fnlrq3ds      kpjbu
024510110103     c                   call      'FNLRQ3R'
024511110103     c                   parm                    kpjba
024512110103     c                   parm                    fidstds
024513110103     c                   movel     kpjbu         fnlrq3ds
024515110103     c                   if        lrq3erro <> ' '
024516110103     c                   eval      v1cmsg = lrq3msgo
024517110103     c                   seton                                        4028
024518110210     c                   leavesr
024519110210     c                   else
024520110210     c                   movel     dstpdr        wpdr
024521110103     c                   end
024700020910      * Il p.o. del padroncino deve essere uguale al p.o. in gestione
024800020910     c                   If        v1cfpd <> v1cfgs
024900101209     c                   Eval      V1cmsg = Msg(07)
025000020910     c                   Eval      *In41 = *On
025100020910     c                   Eval      *In28 = *On
025200110210     c                   leavesr
025300020910     c                   EndIf
025500061002     C*
025600080319     C                   ENDSR
027700061002     C**********************************************************************
027800061002     C*--- INZSR INIZIALE --------------------------------------------*
027900061002     C**********************************************************************
028000061002     C     *INZSR        BEGSR
028100061002     C*
028200061002     C     *ENTRY        PLIST
028300061002     C                   PARM                    KPJBA
028301110211     c                   eval      v03f2 = 'F2=Pistola'
028302110211     c                   setoff                                       36
028400061002      * Reperisco dati job
028500061002     c                   exsr      DatiJob
028600061002     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
028700061002     C                   TIME                    W0140            14 0
028800061002     C                   MOVE      W0140         UDATE8            8 0
028900061002     C                   MOVEL     W0140         HHMM              4 0          *ORA/MINUTI
029000061002     C                   Z-ADD     UDATE8        G02DAT                         *GG/MM/AAAA
029100061002     C                   MOVEL     *BLANK        G02ERR
029200061002     C                   CALL      'XSRDA8'
029300061002     C                   PARM                    WLBDAT
029400061002     C                   Z-ADD     G02INV        DATEU8                         *AAAA/MM/GG
029500061003     C                   MOVEL     dutpou        V1CFGS                         *FIL.GESTIONE
029600061003     C                   MOVEL     dutpou        V1CFPD                         *FIL.PADRONCINO
029700061003     c                   if        dutlpo = '2'
029800061002     C                   SETON                                        05
029900061002    1C                   ENDIF
030000061002     C*
030100110329     C                   MOVEL     'FNLRQ1R'     V1CPGM
030200061003     C                   Z-ADD     4             WNPG              1 0
030300061002     C*
030400061002     C***
030500061002     C* DEFINIZIONE CAMPI
030600061002     C***
030700061002     C     *LIKE         DEFINE    UDATE8        DATEU8
030800110103     C     *LIKE         DEFINE    apdpdr        WPADR
031600101209     c     Kapdw         Klist
031700101209     c                   Kfld                    apdtip
031800101209     c                   Kfld                    wpadr
031801110202     c                   eval      apdtip = 'A'
031802110210     c     Kdst4         Klist
031803110210     c                   Kfld                    apdtip
031805110210     c                   Kfld                    wpadr
031807110210     c                   Kfld                    DSTFCF
031809110202     c     Ktbe1         Klist
031810110202     c                   Kfld                    tbecod
031811110202     c                   Kfld                    tbeke1
031812110202     c     Ktbe2         Klist
031813110202     c                   Kfld                    tbecod
031814110202     c                   Kfld                    tbeke1
031815110202     c                   Kfld                    tbeke2
031900061002     C                   ENDSR
032000061002      *---------------------------------------------------------------*
032100061002      * Reperimento Dati del job (Utente/Operativi)                   *
032200061002      *---------------------------------------------------------------*
032300061002     c     DatiJob       BEGSR
032400061002      *
032500061002     c     *dtaara       define    §azute        azuteds
032600061002     c     *dtaara       define    §datiute      ddatiute
032700061002      *
032800061002     c                   in(E)     *dtaara
032900061002     c                   IF        %ERROR or RSUT = *blanks
033000061002     c                   clear                   Tibs34Ds
033100061002     c                   call      'TIBS34R'
033200061002     c                   parm                    Tibs34Ds
033300061002     c                   in        *dtaara
033400061002     c                   ENDIF
033500061002      *
033600061002     c                   ENDSR
033700950525     C*---------------------------------------------------------------*
033800950523**  MSG
033900970416Foglio Distinta inesistente o in gestione ad altro punto operativo            01
034000110103                                                                              02
034100110103                                                                              03
034200110103                                                                              04
034300950524Foglio Distinta non aperto con questo programma: impossibile la manutenzione  05
034400101209Codice Autotrasportatore inesistente                                          06
034500101209P.O. Autotrasportatore incongruente con punto operativo apertura foglio       07
