000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000101011129     H*--------------------------------------------------------------*
000400000000     H*             - MANUTEZIONE SINISTRI -                         *
000500000000     H*--------------------------------------------------------------*
000600011127     FTRmz26D2  CF   E             WORKSTN
000601011212     Fansif01L  IF   E           K DISK
000700011127     FmzAME01L  IF   E           K DISK
000800011210     FmzSIN00F  UF A E             DISK
000801011210     FmzSIN02l  if   E           k DISK    rename(mzsin000:mzsin002)
000802011203     F                                     INFDS(sinINF)
000803011210     Fmznot01L  iF   E           K DISK
000900011203     FmzIGM00F  UF a E             DISK
000901011203     Fmzigm03l  if   E           k DISK    rename(mzigm000:mzigm003)
000902011203     F                                     INFDS(igmINF)
001400000000     FTABEL00F  IF   E           K DISK
001401011203     D sinINF          DS
001402011203     D  sinNRR               397    400B 0
001403011203     D igmINF          DS
001404011203     D  igmNRR               397    400B 0
001405011127     D*NUNERO SINISTRI
002600011219     D mzNSI           DS
002800000000     D  NUMSIN                 1      7  0
002801011127     D*MANUTENZIONE SINISTRI
002900000000     D PARAM           DS
002901011127     D  parnrf                        7  0
002902011203     D  parnsi                        9  0
002903011203     D  parnig                        9  0
002904011128     D  parflg                        1
002905011127     c* modalità con cui chiamo il pgm
002906011127     D  parmod                        1
002909011203     c* Cmd di ritorno
002910011127     D  parret                        1
002911011127     D*GESTIONE NOTE
007500000000     D PAR191          DS
007501011129     D* numero riferimento
007502011129     D  P91Nrf                        7  0
007503011129     D* tipo nota
007504011129     D  P91tip                        2
007505011129     D* numero nota
007506011129     D  P91NOT                        7  0
007507011127     c* modalità con cui chiamo il pgm
007508011127     D  p91mod                        1
007509011127     c* Cmd di ritorno
007510011127     D  p91ret                        1
009100000000      *
009200000000     D PARUT         E DS                  EXTNAME(UTEDSE0F)
009300000000     D KPJBA         E DS
009301011127      *
009302011127     D DTAiso          S               D   INZ
009303011127     D DTAEUR          S               D   DATFMT(*EUR) INZ
009400000000     C*----------------------------------------------------*
009500000000      * INDICATORI
009600000000      *
009700000000      * KL FINE LAVORO
009800000000      * KA CONFERMA
010000000000      * 30 DESCRIZIONI AGGIUNTIVE
010100000000      * 62 INTERVENTO DEVE ESSERE BLANK SE C'E IL SINISTRO
010200000000      * 63 INTERVENTO ERRATO O MANCANTE
010400000000      * 71 TIPO SINISTRO DEVE ESSERE 'A' O 'P'
010500000000      * 72 PROTEZIONE CAMPI
010600000000      * 75 DATA SINISTRO OBBLIGATORIA
010700000000      * 76 COD.AUTISTA ERRATO
010800000000      * 77 DATA ENTR.OFF.ERRATA
010900000000      * 78 DATA ENTR.OFF.OBBLIGATORIA
011100000000      * 80 COD.OFF.ERRATO
011200000000      * 81 COD.OFF.OBBLIGATORIO
011300000000      * 82 IMMISSIONE
011400000000      * 83 VARIAZIONE
011500000000      * 84 ANNULLAMENTO
011600000000      * 85 DATA USCITA OFFICINA
011700011127      * 97 CAMPI DI mzIGM A 0
011800011210      * 98 CAMPI DI mzSIN A 0
011900000000      * 99 GENERICO DI ERRORE
014600000000     C** PULIZIA
014601011127     c                   if        not *inlr
014700000000     C                   EXSR      PULIZ
017200011127     c* carica campi a video
017202011128     C                   exsr      carica
017204011127     c*
017205011128     c                   do        *hival
017206011128     c                   EXSR      CLR00
017207011128     c                   if        *in99
017208011128     c                   setoff                                       99
017209011128     c                   write     ga26d201
017210011128     c                   seton                                        99
017211011128     c                   end
017212011221     c                   if        *in72 or *in73
017214011128     c                   write     ga26d201
017215011128     c                   EXFMT     ga26d202
017216011128     C                   ELSE
023500000000     C                   EXFMT     GA26D201
023501011128     c                   end
023502011127     C                   SETOFF                                       99
023503011127     C   97              SETOFF                                       97
023800011127     C** FINE LAVORO
023801011127     c                   if        *inkc
023900011127     C                   MOVE      'C'           PARret
023901011129     C** cancello nota se sono in inserimento e ho premuto f3
023902011129     C                   IF        *IN30 AND PARMOD = ' '
023904011129     C                   MOVE      '4'           P91MOD
023905011129     c                   exsr      note
023906011129     C                   END
024100011127     C                   leave
024101011127     c                   end
024102011127     C** ricerca
024103011127     c   kd              exsr      ric01
024300011127     C** DESCRIZIONI AGGIUNTIVE
024500011127     C                   if        *inkh
024501011127     C                   MOVE      PARMOD        P91MOD
024502011221     c                   if        not *in30 and not *in73
024503011221     C                   MOVE      ' '           P91MOD
024504011221     c                   end
025401011127     c                   exsr      note
025402011127     c                   exsr      ricnot
025600000000     C                   END
025700000000     C*--------------------
025800000000     C** CONTROLLI
025900000000     C                   EXSR      CONTR
026000011127     C   99              iter
026400011127     C** CONFERMA
026401011127     c                   if        *inkf
026402011127     C                   MOVE      'F'           PARret
026500000000     C** DESCRIZIONI AGGIUNTIVE
026501011127     C                   IF        *IN30 AND (PARMOD = '4' OR
026503011127     C                             (*IN98 AND *IN97 AND PARMOD = '2'))
026504011127     C                   MOVE      '4'           P91MOD
026701011127     c                   exsr      note
026704011127     C                   END
026705011127     C*
028000000000     C                   EXSR      AGGIOR
028001011127     C* ANNULLAMENTO
028002011128     C*                  IF        PARMOD = '4' OR
028003011128     C*                            (*IN98 AND *IN97 AND PARMOD = '2')
028004011128     C*                  MOVE      'P'           PARret
028005011128     c*                  endif
028006011128     c                   leave
028007011127     c                   endif
028400011127     C                   enddo
028401011128     c                   movel(p)  param         kpjbu
029101011127     c                   endif
029200000000     C                   SETON                                        LR
029201011128     c**********************************************************************
029202011128     c* pulisce 1° formato
029203011128     c**********************************************************************
029204011128     C     clr00         BEGSR
029205011128     C                   Z-ADD     0             §RIGA
029206011128     C                   Z-ADD     0             §COLON
029207011128     C                   CLEAR                   §FLD
029208011128     C                   ENDSR
029300000000     C******************************************************
029301011127     C* DESCRIZIONI AGGIUNTIVE
029401011127     C******************************************************
029402011127     C     ricnot        BEGSR
029403011127     c*
029404011210     c     knot          chain     mznot01l
029405011210     c                   if        %found(mznot01l)
029406011127     C                   SETON                                        30
029409011127     C                   END
029410011127     c*
029411011127     c                   endsr
029412011127     C******************************************************
029413011127     C* DESCRIZIONI AGGIUNTIVE
029415011127     C******************************************************
029416011127     C     note          BEGSR
029421011127     c*
029424011129     C                   Z-ADD     vnrf          P91nrf
029427011127     C                   MOVEL(p)  PAR191        KPJBU
029428011129     C                   CALL      'TRMZ26R3'
029429011127     C                   PARM                    KPJBA
029430011127     C                   MOVEL     KPJBU         PAR191
029432011129     c* se sono in modifica aggiorno il record
029433011129     c                   if        parmod = '2'
029434011210     C     parnsi        CHAIN     mzSIN00F                           21
029435011129     c                   if        not *in21
029443011129     C                   MOVE      P91NOT        SINNOT
029444011210     C                   UPDATE    mzSIN000
029445011129     c                   end
029451011203     C     parnig        CHAIN     mzIGM00f                           21
029452011129     c                   if        not *in21
029474011129     C                   Z-ADD     P91NOT        IGMNOT
029475011129     C                   UPDATE    mzIGM000
029476011129     c                   end
029477011129     c                   end
029478011127     c*
029479011127     c                   endsr
029480011127     C******************************************************
029481011127     C     carica        BEGSR
029482011127     C******************************************************
029483011127     C*
029484011128     c                   do
029485011128     C                   MOVEL     PARNRF        Vnrf
029486011127     c     parnrf        chain     mzame01l
029487011128     c                   if        %found(mzame01l)
029488011128     C                   MOVEL     amedit        Vdit
029489011212     C     vdit          CHAIN     ansif01L
029490011212     C                   if        %found(ansif01l)
029491011212     C                   MOVEL     sifdesbrev    Vddit
029492011212     c                   else
029493011212     C                   MOVEL     *blanks       Vddit
029494011212     c                   end
029495011128     C                   MOVEL     ametga        Vtga
029496011128     C                   MOVEL     ametgn        Vtgn
029497011128     C                   MOVEL     amemar        Vmar
029498011128     C                   MOVEL     ametip        VTIP
029499011203     c                   else
029500011203     C                   MOVEL     'STORICO'     VTGN
029502011128     c                   end
029503011128     c* modifica
029504011128     c                   if        parmod <> ' '
029505011203     c                   if        parnsi <> 0
029506011210     c     parnsi        chain     mzsin00F                           21
029507011203     c     kigm          chain     mzigm03l                           22
029508011203     c                   end
029509011203     c                   if        parnig <> 0
029510011203     c     parnig        chain     mzigm00f                           22
029511011210     c     igmnsi        chain     mzsin02l                           21
029512011203     c                   end
029513011128     c                   if        *in21 and *in22
029514011128     c                   seton                                        lr
029515011128     c                   leave
029516011128     c                   end
029517011128     c                   if        not *in22
029518011203     c                   if        parnig = 0
029519011203     C                   z-add     igmnrr        parnig
029520011203     c                   end
029521011203     C                   MOVE      IGMNOT        P91NOT
029522011203     C                   MOVE      igmINT        VINT
029523011128     C                   MOVE      igmOFF        VOFF
029524011128     C                   MOVE      igmOFb        VOFB
029525011127     C                   MOVE      igmdat        dtaiso
029526011127     C                   MOVE      dtaiso        dtaeur
029527011127     C                   MOVE      dtaeur        VDAT
029528011127     C                   z-add     igmkm         VKM
029529011128     c                   if        igmdtf <> 0
029530011128     C                   MOVE      igmdtf        dtaiso
029531011128     C                   MOVE      dtaiso        dtaeur
029532011128     C                   MOVE      dtaeur        VDtf
029533011128     c                   end
029534011128     c                   z-add     igmipv        vopv
029535011128     c                   z-add     igmief        voef
029536011128     c                   end
029537011128     c                   if        not *in21
029538011203     c                   if        parnsi = 0
029539011203     C                   z-add     sinnrr        parnsi
029540011203     c                   end
029541011128     C                   MOVE      sinNOT        P91NOT
029542011128     C                   MOVE      sinDSI        dtaiso
029543011128     C                   MOVE      dtaiso        dtaeur
029544011128     C                   MOVE      dtaeur        VDSI
029545011128     C                   MOVE      sinaus        Vaus
029546011128     C                   MOVE      sinatt        Vatt
029547011128     C                   MOVE      sinde1        Vde1
029548011128     C                   MOVE      sinde2        Vde2
029549011128     c                   end
029550011127     c* DECODIFICHE
029551011127     C                   EXSR      CONTR
029552011127     c* ricerca note
029553011127     C                   EXSR      ricnot
029554011127     C                   END
029555011128     C                   ENDdo
029556011127     c*
029557011127     c                   endsr
029558011127     C******************************************************
029600000000     C** PULIZIA CAMPI
029601011127     C******************************************************
029701011127     C     PULIZ         BEGSR
029900011128     C                   SETOFF                                       9798
030000000000     C                   Z-ADD     0             NUMSIN
030100000000     C                   Z-ADD     0             VNRF
030200000000     C                   MOVEL     *BLANKS       VTGA
030300000000     C                   MOVEL     *BLANKS       VTGN
030400000000     C                   MOVEL     *BLANKS       VMAR
030500000000     C                   MOVEL     *BLANKS       VTIP
030700000000     C                   Z-ADD     0             VDSI
030800000000     C                   MOVEL     *BLANKS       VAUS
030801011128     C                   MOVEL     *BLANKS       VdAUS
030900000000     C                   MOVEL     *BLANKS       VATT
031000000000     C                   MOVEL     *BLANKS       VDE1
031100000000     C                   MOVEL     *BLANKS       VDE2
031200000000     C                   Z-ADD     0             VDAT
031300000000     C                   Z-ADD     0             VKM
031400000000     C                   Z-ADD     0             VDTF
031500000000     C                   MOVEL     *BLANKS       VOFB
031501011128     C                   MOVEL     *BLANKS       VdOFB
031600000000     C                   MOVEL     *BLANKS       VINT
031601011128     C                   MOVEL     *BLANKS       VINTd
031700000000     C                   MOVEL     *BLANKS       VOFF
031800000000     C                   Z-ADD     0             VOPV
031900011128     c                   Z-ADD     0             VOEF
031901011128     c                   Z-ADD     0             §RIG
031902011128     c                   Z-ADD     0             §COL
032000000000     C                   ENDSR
032100000000     C******************************************************
032101011127     C** CONTROLLI VIDEATA
032102011127     C******************************************************
032200000000     C     CONTR         BEGSR
032300000000     C                   SETOFF                                         9798
032301011127     c                   movel     ' '           fld               1
032302011127     C                   do
032700011127     C* SE I CAMPI SONO PROTETTI salta i controlli
032801011221     c                   if        *in72 or *in73
032802011128     c                   Z-ADD     0             §RIG
032803011128     c                   Z-ADD     0             §COL
032804011128     c                   leave
032805011128     c                   end
033200011130     C*
033300000000     C     VDAT          IFEQ      0
033301011130     C     Vkm           andeq     0
033500000000     C     VDTF          ANDEQ     0
033600000000     C     VOFB          ANDEQ     *BLANKS
033700000000     C     VINT          ANDEQ     *BLANKS
033800000000     C     VOPV          ANDEQ     0
033900000000     C     VOEF          ANDEQ     0
033901011129     C                   MOVEL     *BLANKS       VINTD
033902011129     C                   MOVEL     *BLANKS       VDOFB
034000000000     C                   SETON                                        97
034100000000     C                   END
034200011130     C*
034500000000     C     VDSI          IFEQ      0
034600000000     C     VATT          ANDEQ     *BLANKS
034700000000     C     VAUS          ANDEQ     *BLANKS
034800000000     C     VDE1          ANDEQ     *BLANKS
034900000000     C     VDE2          ANDEQ     *BLANKS
034901011129     C                   MOVEL     *BLANKS       VDAUS
035000000000     C                   SETON                                        98
035100000000     C                   END
035200011127     C                   if        not *in98
035300000000     C** DATA SINISTRO
035400011127     C     *eur          test(d)                 vdsi                   75
035401011127     c                   if        *in75
035800011127     C                   SETON                                        99
035900011127     C                   leave
036000000000     C                   ELSE
036100011127     C                   move      vdsi          dtaeur
036200011127     C                   MOVE      dtaeur        dtaiso
036400011127     C                   MOVE      dtaiso        SINDSI
036800000000     C                   END
036801011127     c* autista
036802011127     c                   exsr      srvaus
036803011128     c   99              leave
038800000000     C* TIPO SINISTRO 'A' O 'P'
039000000000     C     VATT          IFNE      'A'
039100000000     C     VATT          ANDNE     'P'
039200000000     C                   SETON                                        7199
039300011127     C                   leave
039400000000     C                   END
039501011127     C                   END
039600000000     C*--
039700000000     C* CAMPI MANUTENZIONE
039800000000     C*--
040000000000      * SE REGISTRO IL SINISTRO VINT DEVE ESSERE '  '
040200011127     C                   If        not *in98 and vint <> *BLANK
040300000000     C                   SETON                                        6299
040400011127     C                   leave
040500000000     C                   END
040501011127     c*
040700000000     C                   MOVEL     *BLANK        VINTD
040800011127     C                   if        not *in97
040801011128     c*controllo l'intervento solo se no sinistro
040900011127     C                   if        *in98
041000000000     C     VINT          IFEQ      *BLANKS
041100000000     C                   SETON                                        6399
041200011127     C                   leave
041300000000     C                   END
041301011127     c                   exsr      srvint
041302011128     C   99              leave
041304011128     C                   END
043000011128     C** DATA autorizzazione
043100000000     C     VDAT          IFEQ      0
043200000000     C                   SETON                                        7899
043300011127     C                   leave
043400000000     C                   ELSE
043500011127     C     *eur          test(d)                 vdat                   77
043501011127     c                   if        *in77
043900011127     C                   SETON                                        99
044000011127     C                   leave
044100000000     C                   END
044200000000     C                   END
044800011128     C** DATA fattura
044900000000     C     VDTF          IFNE      0
044901011127     C     *eur          test(d)                 vdtf                   85
044902011127     c                   if        *in85
044903011127     C                   SETON                                        99
044904011127     C                   leave
044905011127     C                   END
045700000000     C                   END
045800000000     C** OFFICINA
045900000000     C     VOFB          IFEQ      *BLANKS
046000000000     C                   SETON                                        8199
046100011127     C                   leave
046200000000     C                   END
046201011127     c                   exsr      srvofb
046202011128     c   99              leave
046203011128     C                   END
048100011127     C                   ENDdo
048200011127     C                   ENDSR
048300000000     C******************************************************
048301011127     c* ricerca
048302011127     C******************************************************
048400011127     C     ric01         BEGSR
048401011127     c                   movel     '1'           fld
048402011127     c                   select
048403011127     C*COD.autista
048404011127     C                   when      §fld = 'VAUS'
048405011127     c                   exsr      srvaus
048406011127     C*COD.interventi
048407011127     C                   when      §fld = 'VINT'
048408011127     c                   exsr      srvint
048409011127     C*COD.officina
048410011127     C                   when      §fld = 'VOFB'
048411011127     c                   exsr      srvofb
048412011127     C                   endsl
048413011127     C* POSIZIONAMENTO CURSORE
048414011127     C                   Z-ADD     §RIGA         §RIG
048415011127     C                   Z-ADD     §COLON        §COL
048416011127     C                   ENDSR
048417011127     c**********************************************************************
048418011127     C** autista
048419011127     c**********************************************************************
048420011127     C     srvaus        BEGSR
048421011127     c*
048422011127     c                   movel     'XN'          parcod
048423011127     c                   movel(P)  vaus          parkey
048424011127     c                   exsr      srtab
048425011127     c                   movel     parkey        vaus
048426011127     c                   if        *in99
048427011127     C                   MOVEL     *blanks       VDaus
048428011127     C                   SETON                                        76
048429011127     C                   else
048430011127     C                   MOVEL     TBLUNI        VDaus
048431011127     C                   END
048432011127     c*
048433011127     C                   ENDSR
048434011127     c**********************************************************************
048435011127     C** intervento
048436011127     c**********************************************************************
048437011127     C     srvint        BEGSR
048438011127     c*
048439011127     c                   movel     'XE'          parcod
048440011127     c                   movel(P)  vint          parkey
048441011127     c                   exsr      srtab
048442011127     c                   movel     parkey        vint
048443011127     c                   if        *in99
048444011127     C                   MOVEL     *blanks       Vintd
048445011127     C                   SETON                                        63
048446011127     C                   else
048447011127     C                   MOVEL     TBLUNI        Vintd
048448011127     C                   END
048449011127     c*
048450011127     C                   ENDSR
048451011127     c**********************************************************************
048452011127     C** officina
048453011127     c**********************************************************************
048454011127     C     srvofb        BEGSR
048455011127     c*
048456011127     c                   movel     'XO'          parcod
048457011127     c                   movel(P)  vofb          parkey
048458011127     c                   exsr      srtab
048459011127     c                   movel     parkey        vofb
048460011127     c                   if        *in99
048461011127     C                   MOVEL     *blanks       Vdofb
048462011127     C                   SETON                                        80
048463011127     C                   else
048464011127     C                   MOVEL     TBLUNI        Vdofb
048465011127     C                   END
048466011127     c*
048467011127     C                   ENDSR
048468011127     C**********************************************************************
048469011127     C** Controlli e decodifiche campi con tabel00f
048470011127     C**********************************************************************
048471011127     C     srtab         BEGSR
048472011127     c                   if        fld <> *blanks
048473011127     c                   movel     parkey        parkeys           8
048474011127     C                   CALL      'TRUL19R'
048475011127     c                   parm                    parcod            2
048476011127     c                   parm      '1'           parord            1
048477011127     c                   parm      *blanks       parkey            8
048478011127     c                   parm      *blanks       cmd               1
048479011127     c* ritorno senza scelta
048480011127     c                   if        cmd = 'L'
048483011127     c                   movel     parkeys       parkey
048484011127     C                   END
048485011127     C                   END
048486011127     C                   MOVEL     *blanks       tbluni
048487011127     C                   MOVEL     parcod        COD
048488011127     C                   MOVEL(p)  parkey        KEY
048489011128     C     KTAB          CHAIN     TABEL00f                           99
048490011127     c*
048491011127     C                   ENDSR
048492011127     C******************************************************
048493011127     C** AGGIORNAMENTI FILE
048494011127     C******************************************************
048495011127     C     AGGIOR        BEGSR
048700000000     C                   MOVE      VNRF          IGMNRF
048800000000     C                   MOVE      VINT          IGMINT
048900000000     C                   MOVE      VKM           IGMKM
048901011127     c                   do
049300011210     C** DELET  DI mzSIN E mzIGM
049400011127     C                   if        *in84
049500011210     C     parnsi        DELETE    mzSIN00F                           21
049600011203     C     parnig        DELETE    mzIGM00f                           21
049900011127     C                   leave
050000000000     C                   END
050100011210     C** SE I CAMPI DI mzIGM SONO TUTTI A 0 delete di mzsin e mzigm
050300000000     C** ALTRIMENTI UPDAT   SU TUTTI E 2
050400011127     C                   if        *in83
050500000000     C  N98              EXSR      AGGSIN
050600011210     C   98parnsi        DELETE    mzSIN00F                           21
050900000000     C  N97              EXSR      AGGIGM
051000011203     C   97parnig        DELETE    mzIGM00f                           21
051200000000     C                   END
051300011210     C** WRITE mzSIN/mzIGM
051400011127     C                   if        *in82
051600000000     C  N98              EXSR      AGGSIN
051800000000     C  N97              EXSR      AGGIGM
051900000000     C                   END
051901011127     C                   ENDdo
052000011127     C                   ENDSR
052100000000     C******************************************************
052101011210     C** mzSIN00F
052102011127     C******************************************************
052200000000     C     AGGSIN        BEGSR
052600000000     C* INCREMENTO N.SINISTRO
052700011203     C     parNSI        IFEQ      0
052800011219     C     *LOCK         IN        mzNSI
052900000000     C                   ADD       1             NUMSIN
053000011219     C                   OUT       mzNSI
053100000000     C                   Z-ADD     NUMSIN        SINNSI
053200000000     C                   END
053300000000     C**------------------------
053400011210     C     parnsi        CHAIN     mzSIN00F                           21
053500000000     C                   Z-ADD     VNRF          SINNRF
053600011127     C                   move      VDSI          Dtaeur
053601011127     C                   move      dtaeur        Dtaiso
053800011127     C                   MOVE      Dtaiso        SINDSI
054500000000     C                   MOVEL     VAUS          SINAUS
054600000000     C                   MOVEL     VATT          SINATT
054700000000     C                   MOVEL     VDE1          SINDE1
054800000000     C                   MOVEL     VDE2          SINDE2
054900000000     C                   MOVE      P91NOT        SINNOT
055000011210     C  N21              UPDATE    mzSIN000
055100011210     C   21              WRITE     mzSIN000
055200000000     C                   ENDSR
055300000000     C******************************************************
055301011128     C** mzIGM00F
055302011127     C******************************************************
055400000000     C     AGGIGM        BEGSR
055800011203     C     parnig        CHAIN     mzIGM00f                           21
055900000000     C                   Z-ADD     VNRF          IGMNRF
055901011127     C                   move      VDat          Dtaeur
055902011127     C                   move      dtaeur        Dtaiso
055903011127     C                   MOVE      Dtaiso        igmdat
056900000000     C                   Z-ADD     VKM           IGMKM
057000000000     C                   MOVE      VINT          IGMINT
057100000000     C     VDTF          IFNE      0
057101011127     C                   move      VDtf          Dtaeur
057102011127     C                   move      dtaeur        Dtaiso
057103011127     C                   MOVE      Dtaiso        igmdtf
058100000000     C                   ELSE
058200011128     C                   Z-ADD     0             IGMDTF
058300000000     C                   END
058400000000     C                   MOVEL     VINT          IGMINT
058600000000     C                   Z-ADD     0             IGMDIT
058700000000     C                   Z-ADD     0             IGMKCO
058800000000     C                   Z-ADD     0             IGMSCO
058900000000     C                   MOVEL     VOFF          IGMOFF
059000000000     C                   MOVEL     VOFB          IGMOFB
059100011128     C                   z-add     VOPV          IGMIPV
059200011128     C                   z-add     VOEF          IGMIEF
059300000000     C                   Z-ADD     SINNSI        IGMNSI
059400000000     C                   Z-ADD     P91NOT        IGMNOT
059500011128     C  N21              UPDATE    mzIGM000
059600011128     C   21              WRITE     mzIGM000
059800000000     C                   ENDSR
065601011127     C*----------------------------------------------------*
065602011127     c     *inzsr        begsr
065603011127     C*----------------------------------------------------*
065604011127     C* POSIZIONAMENTO TABELLE                             *
065605011127     C     KTAB          KLIST                                                  *
065606011127     C                   KFLD                    CODUT                          *
065607011127     C                   KFLD                    COD               2            *
065608011127     C                   KFLD                    KEY               8            *
065616011210     C* mznot01L
065617011127     C     Knot          KLIST                                                  *
065618011127     C                   KFLD                    nottip                         *
065619011203     C                   KFLD                    P91not                         *
065620011203     C* mzigm03l
065621011203     C     Kigm          KLIST                                                  *
065622011203     C                   KFLD                    sinnrf                         *
065623011203     C                   KFLD                    SINnsi                         *
065624011127     C     *ENTRY        PLIST                                                  *
065625011127     C                   PARM                    KPJBA                          *
065626011127     C                   PARM                    TIPO             15            *
065627011127     C                   MOVE      1             CODUT
065628011127     C                   CALL      'XPARUT'
065629011127     C                   PARM                    PARUT
065630011127     C*--------- SE DITTA INESISTENTE FINE LAVORO          *
065631011127     C                   if        codut = -1
065632011127     c                   seton                                        lr
065633011127     c                   end
065634011127     C                   MOVEL     RAGUT         RSUT             20
065635011127     C                   MOVEL     KPJBU         PARAM
065636011128     c                   clear                   par191
065637011127     C** IMMISSIONE
065638011127     C     parmod        IFEQ      *BLANKS
065639011127     C                   SETON                                        82
065640011127     C                   END
065641011127     C** VARIAZIONE
065642011127     C     parmod        IFEQ      '2'
065643011127     C                   SETON                                        83
065644011127     C                   END
065645011127     C** ANNULLAMENTO
065646011127     C     parmod        IFEQ      '4'
065647011127     C                   SETON                                        8472
065648011127     C                   END
065649011221     C** VISUALIZZAZIONE
065650011221     C     parmod        IFEQ      '5'
065651011221     C                   SETON                                        73
065652011221     C                   END
065653011128     C** PROGRAMMA CHIAMANTE
065654011128     C     PARFLg        COMP      '1'                                    70
065655011219     C     *DTAARA       DEFINE                  mzNSI
06570000000005050C                   ENDSR
