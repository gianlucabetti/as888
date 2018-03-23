000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200011129     H*--------------------------------------------------------------*
000300000000     H*             - MANUTEZIONE SINISTRI -                         *
000400000000     H*--------------------------------------------------------------*
000500011127     FTRmz26D2  CF   E             WORKSTN
000600011212     Fansif01L  IF   E           K DISK
000700011127     FmzAME01L  IF   E           K DISK
000800011210     FmzSIN00F  UF A E             DISK
000900011210     FmzSIN02l  if   E           k DISK    rename(mzsin000:mzsin002)
001000011203     F                                     INFDS(sinINF)
001100011210     Fmznot01L  iF   E           K DISK
001200011203     FmzIGM00F  UF a E             DISK
001300011203     Fmzigm03l  if   E           k DISK    rename(mzigm000:mzigm003)
001400011203     F                                     INFDS(igmINF)
001500000000     FTABEL00F  IF   E           K DISK
001600160518     FTntbe01l  IF   E           K DISK
001700011203     D sinINF          DS
001800011203     D  sinNRR               397    400B 0
001900011203     D igmINF          DS
002000011203     D  igmNRR               397    400B 0
002100011127     D*NUNERO SINISTRI
002200011219     D mzNSI           DS
002300000000     D  NUMSIN                 1      7  0
002400011127     D*MANUTENZIONE SINISTRI
002500000000     D PARAM           DS
002600011127     D  parnrf                        7  0
002700011203     D  parnsi                        9  0
002800011203     D  parnig                        9  0
002900011128     D  parflg                        1
003000011127     c* modalità con cui chiamo il pgm
003100011127     D  parmod                        1
003200011203     c* Cmd di ritorno
003300011127     D  parret                        1
003400011127     D*GESTIONE NOTE
003500000000     D PAR191          DS
003600011129     D* numero riferimento
003700011129     D  P91Nrf                        7  0
003800011129     D* tipo nota
003900011129     D  P91tip                        2
004000011129     D* numero nota
004100011129     D  P91NOT                        7  0
004200011127     c* modalità con cui chiamo il pgm
004300011127     D  p91mod                        1
004400011127     c* Cmd di ritorno
004500011127     D  p91ret                        1
004600000000      *
004700000000     D PARUT         E DS                  EXTNAME(UTEDSE0F)
004800000000     D KPJBA         E DS
004900011127      *
005000160518     D dmau          E DS
005100160518     D dcau          E DS
005200011127     D DTAiso          S               D   INZ
005300011127     D DTAEUR          S               D   DATFMT(*EUR) INZ
005400000000     C*----------------------------------------------------*
005500000000      * INDICATORI
005600000000      *
005700000000      * KL FINE LAVORO
005800000000      * KA CONFERMA
005900000000      * 30 DESCRIZIONI AGGIUNTIVE
006000000000      * 62 INTERVENTO DEVE ESSERE BLANK SE C'E IL SINISTRO
006100000000      * 63 INTERVENTO ERRATO O MANCANTE
006200000000      * 71 TIPO SINISTRO DEVE ESSERE 'A' O 'P'
006300000000      * 72 PROTEZIONE CAMPI
006400000000      * 75 DATA SINISTRO OBBLIGATORIA
006500000000      * 76 COD.AUTISTA ERRATO
006600000000      * 77 DATA ENTR.OFF.ERRATA
006700000000      * 78 DATA ENTR.OFF.OBBLIGATORIA
006800000000      * 80 COD.OFF.ERRATO
006900000000      * 81 COD.OFF.OBBLIGATORIO
007000000000      * 82 IMMISSIONE
007100000000      * 83 VARIAZIONE
007200000000      * 84 ANNULLAMENTO
007300000000      * 85 DATA USCITA OFFICINA
007400011127      * 97 CAMPI DI mzIGM A 0
007500011210      * 98 CAMPI DI mzSIN A 0
007600000000      * 99 GENERICO DI ERRORE
007700000000     C** PULIZIA
007800011127     c                   if        not *inlr
007900000000     C                   EXSR      PULIZ
008000011127     c* carica campi a video
008100011128     C                   exsr      carica
008200011127     c*
008300011128     c                   do        *hival
008400011128     c                   EXSR      CLR00
008500011128     c                   if        *in99
008600011128     c                   setoff                                       99
008700011128     c                   write     ga26d201
008800011128     c                   seton                                        99
008900011128     c                   end
009000011221     c                   if        *in72 or *in73
009100011128     c                   write     ga26d201
009200011128     c                   EXFMT     ga26d202
009300011128     C                   ELSE
009400000000     C                   EXFMT     GA26D201
009500011128     c                   end
009600011127     C                   SETOFF                                       99
009700011127     C   97              SETOFF                                       97
009800011127     C** FINE LAVORO
009900011127     c                   if        *inkc
010000011127     C                   MOVE      'C'           PARret
010100011129     C** cancello nota se sono in inserimento e ho premuto f3
010200011129     C                   IF        *IN30 AND PARMOD = ' '
010300011129     C                   MOVE      '4'           P91MOD
010400011129     c                   exsr      note
010500011129     C                   END
010600011127     C                   leave
010700011127     c                   end
010800011127     C** ricerca
010900011127     c   kd              exsr      ric01
011000011127     C** DESCRIZIONI AGGIUNTIVE
011100011127     C                   if        *inkh
011200011127     C                   MOVE      PARMOD        P91MOD
011300011221     c                   if        not *in30 and not *in73
011400011221     C                   MOVE      ' '           P91MOD
011500011221     c                   end
011600011127     c                   exsr      note
011700011127     c                   exsr      ricnot
011800000000     C                   END
011900000000     C*--------------------
012000000000     C** CONTROLLI
012100000000     C                   EXSR      CONTR
012200011127     C   99              iter
012300011127     C** CONFERMA
012400011127     c                   if        *inkf
012500011127     C                   MOVE      'F'           PARret
012600000000     C** DESCRIZIONI AGGIUNTIVE
012700011127     C                   IF        *IN30 AND (PARMOD = '4' OR
012800011127     C                             (*IN98 AND *IN97 AND PARMOD = '2'))
012900011127     C                   MOVE      '4'           P91MOD
013000011127     c                   exsr      note
013100011127     C                   END
013200011127     C*
013300000000     C                   EXSR      AGGIOR
013400011127     C* ANNULLAMENTO
013500011128     C*                  IF        PARMOD = '4' OR
013600011128     C*                            (*IN98 AND *IN97 AND PARMOD = '2')
013700011128     C*                  MOVE      'P'           PARret
013800011128     c*                  endif
013900011128     c                   leave
014000011127     c                   endif
014100011127     C                   enddo
014200011128     c                   movel(p)  param         kpjbu
014300011127     c                   endif
014400000000     C                   SETON                                        LR
014500011128     c**********************************************************************
014600011128     c* pulisce 1° formato
014700011128     c**********************************************************************
014800011128     C     clr00         BEGSR
014900011128     C                   Z-ADD     0             §RIGA
015000011128     C                   Z-ADD     0             §COLON
015100011128     C                   CLEAR                   §FLD
015200011128     C                   ENDSR
015300000000     C******************************************************
015400011127     C* DESCRIZIONI AGGIUNTIVE
015500011127     C******************************************************
015600011127     C     ricnot        BEGSR
015700011127     c*
015800011210     c     knot          chain     mznot01l
015900011210     c                   if        %found(mznot01l)
016000011127     C                   SETON                                        30
016100011127     C                   END
016200011127     c*
016300011127     c                   endsr
016400011127     C******************************************************
016500011127     C* DESCRIZIONI AGGIUNTIVE
016600011127     C******************************************************
016700011127     C     note          BEGSR
016800011127     c*
016900011129     C                   Z-ADD     vnrf          P91nrf
017000011127     C                   MOVEL(p)  PAR191        KPJBU
017100011129     C                   CALL      'TRMZ26R3'
017200011127     C                   PARM                    KPJBA
017300011127     C                   MOVEL     KPJBU         PAR191
017400011129     c* se sono in modifica aggiorno il record
017500011129     c                   if        parmod = '2'
017600011210     C     parnsi        CHAIN     mzSIN00F                           21
017700011129     c                   if        not *in21
017800011129     C                   MOVE      P91NOT        SINNOT
017900011210     C                   UPDATE    mzSIN000
018000011129     c                   end
018100011203     C     parnig        CHAIN     mzIGM00f                           21
018200011129     c                   if        not *in21
018300011129     C                   Z-ADD     P91NOT        IGMNOT
018400011129     C                   UPDATE    mzIGM000
018500011129     c                   end
018600011129     c                   end
018700011127     c*
018800011127     c                   endsr
018900011127     C******************************************************
019000011127     C     carica        BEGSR
019100011127     C******************************************************
019200011127     C*
019300011128     c                   do
019400011128     C                   MOVEL     PARNRF        Vnrf
019500011127     c     parnrf        chain     mzame01l
019600011128     c                   if        %found(mzame01l)
019700011128     C                   MOVEL     amedit        Vdit
019800011212     C     vdit          CHAIN     ansif01L
019900011212     C                   if        %found(ansif01l)
020000011212     C                   MOVEL     sifdesbrev    Vddit
020100011212     c                   else
020200011212     C                   MOVEL     *blanks       Vddit
020300011212     c                   end
020400011128     C                   MOVEL     ametga        Vtga
020500011128     C                   MOVEL     ametgn        Vtgn
020600160518     C                   MOVEL     AMEmau        VMAu
020700160518     C                   MOVEL     AMEcaut       Vcaut
020800160518     c                   movel     'MAU'         tbecod
020900160518     c                   movel(p)  amemau        tbeke1
021000160518     c     ktbe          chain     tntbe01l
021100160518     c                   if        %found(tntbe01l)
021200160518     c                   movel     tbeuni        dmau
021300160518     c                   movel     §maudes       vmaud
021400160518     c                   endif
021500160518     c                   movel     'CAU'         tbecod
021600160518     c                   movel(p)  amecaut       tbeke1
021700160518     c     ktbe          chain     tntbe01l
021800160518     c                   if        %found(tntbe01l)
021900160518     c                   movel     tbeuni        dcau
022000160518     c                   movel     §caudes       vcautd
022100160518     c                   endif
022200011203     c                   else
022300011203     C                   MOVEL     'STORICO'     VTGN
022400011128     c                   end
022500011128     c* modifica
022600011128     c                   if        parmod <> ' '
022700011203     c                   if        parnsi <> 0
022800011210     c     parnsi        chain     mzsin00F                           21
022900011203     c     kigm          chain     mzigm03l                           22
023000011203     c                   end
023100011203     c                   if        parnig <> 0
023200011203     c     parnig        chain     mzigm00f                           22
023300011210     c     igmnsi        chain     mzsin02l                           21
023400011203     c                   end
023500011128     c                   if        *in21 and *in22
023600011128     c                   seton                                        lr
023700011128     c                   leave
023800011128     c                   end
023900011128     c                   if        not *in22
024000011203     c                   if        parnig = 0
024100011203     C                   z-add     igmnrr        parnig
024200011203     c                   end
024300011203     C                   MOVE      IGMNOT        P91NOT
024400011203     C                   MOVE      igmINT        VINT
024500011128     C                   MOVE      igmOFF        VOFF
024600011128     C                   MOVE      igmOFb        VOFB
024700011127     C                   MOVE      igmdat        dtaiso
024800011127     C                   MOVE      dtaiso        dtaeur
024900011127     C                   MOVE      dtaeur        VDAT
025000011127     C                   z-add     igmkm         VKM
025100011128     c                   if        igmdtf <> 0
025200011128     C                   MOVE      igmdtf        dtaiso
025300011128     C                   MOVE      dtaiso        dtaeur
025400011128     C                   MOVE      dtaeur        VDtf
025500011128     c                   end
025600011128     c                   z-add     igmipv        vopv
025700011128     c                   z-add     igmief        voef
025800011128     c                   end
025900011128     c                   if        not *in21
026000011203     c                   if        parnsi = 0
026100011203     C                   z-add     sinnrr        parnsi
026200011203     c                   end
026300011128     C                   MOVE      sinNOT        P91NOT
026400011128     C                   MOVE      sinDSI        dtaiso
026500011128     C                   MOVE      dtaiso        dtaeur
026600011128     C                   MOVE      dtaeur        VDSI
026700011128     C                   MOVE      sinaus        Vaus
026800011128     C                   MOVE      sinatt        Vatt
026900011128     C                   MOVE      sinde1        Vde1
027000011128     C                   MOVE      sinde2        Vde2
027100011128     c                   end
027200011127     c* DECODIFICHE
027300011127     C                   EXSR      CONTR
027400011127     c* ricerca note
027500011127     C                   EXSR      ricnot
027600011127     C                   END
027700011128     C                   ENDdo
027800011127     c*
027900011127     c                   endsr
028000011127     C******************************************************
028100000000     C** PULIZIA CAMPI
028200011127     C******************************************************
028300011127     C     PULIZ         BEGSR
028400011128     C                   SETOFF                                       9798
028500000000     C                   Z-ADD     0             NUMSIN
028600000000     C                   Z-ADD     0             VNRF
028700000000     C                   MOVEL     *BLANKS       VTGA
028800000000     C                   MOVEL     *BLANKS       VTGN
028900160518     C                   MOVEL     *BLANKS       VMAu
029000160518     C                   MOVEL     *BLANKS       Vcaut
029100000000     C                   Z-ADD     0             VDSI
029200000000     C                   MOVEL     *BLANKS       VAUS
029300011128     C                   MOVEL     *BLANKS       VdAUS
029400000000     C                   MOVEL     *BLANKS       VATT
029500000000     C                   MOVEL     *BLANKS       VDE1
029600000000     C                   MOVEL     *BLANKS       VDE2
029700000000     C                   Z-ADD     0             VDAT
029800000000     C                   Z-ADD     0             VKM
029900000000     C                   Z-ADD     0             VDTF
030000000000     C                   MOVEL     *BLANKS       VOFB
030100011128     C                   MOVEL     *BLANKS       VdOFB
030200000000     C                   MOVEL     *BLANKS       VINT
030300011128     C                   MOVEL     *BLANKS       VINTd
030400000000     C                   MOVEL     *BLANKS       VOFF
030500000000     C                   Z-ADD     0             VOPV
030600011128     c                   Z-ADD     0             VOEF
030700011128     c                   Z-ADD     0             §RIG
030800011128     c                   Z-ADD     0             §COL
030900000000     C                   ENDSR
031000000000     C******************************************************
031100011127     C** CONTROLLI VIDEATA
031200011127     C******************************************************
031300000000     C     CONTR         BEGSR
031400000000     C                   SETOFF                                         9798
031500011127     c                   movel     ' '           fld               1
031600011127     C                   do
031700011127     C* SE I CAMPI SONO PROTETTI salta i controlli
031800011221     c                   if        *in72 or *in73
031900011128     c                   Z-ADD     0             §RIG
032000011128     c                   Z-ADD     0             §COL
032100011128     c                   leave
032200011128     c                   end
032300011130     C*
032400000000     C     VDAT          IFEQ      0
032500011130     C     Vkm           andeq     0
032600000000     C     VDTF          ANDEQ     0
032700000000     C     VOFB          ANDEQ     *BLANKS
032800000000     C     VINT          ANDEQ     *BLANKS
032900000000     C     VOPV          ANDEQ     0
033000000000     C     VOEF          ANDEQ     0
033100011129     C                   MOVEL     *BLANKS       VINTD
033200011129     C                   MOVEL     *BLANKS       VDOFB
033300000000     C                   SETON                                        97
033400000000     C                   END
033500011130     C*
033600000000     C     VDSI          IFEQ      0
033700000000     C     VATT          ANDEQ     *BLANKS
033800000000     C     VAUS          ANDEQ     *BLANKS
033900000000     C     VDE1          ANDEQ     *BLANKS
034000000000     C     VDE2          ANDEQ     *BLANKS
034100011129     C                   MOVEL     *BLANKS       VDAUS
034200000000     C                   SETON                                        98
034300000000     C                   END
034400011127     C                   if        not *in98
034500000000     C** DATA SINISTRO
034600011127     C     *eur          test(d)                 vdsi                   75
034700011127     c                   if        *in75
034800011127     C                   SETON                                        99
034900011127     C                   leave
035000000000     C                   ELSE
035100011127     C                   move      vdsi          dtaeur
035200011127     C                   MOVE      dtaeur        dtaiso
035300011127     C                   MOVE      dtaiso        SINDSI
035400000000     C                   END
035500011127     c* autista
035600011127     c                   exsr      srvaus
035700011128     c   99              leave
035800000000     C* TIPO SINISTRO 'A' O 'P'
035900000000     C     VATT          IFNE      'A'
036000000000     C     VATT          ANDNE     'P'
036100000000     C                   SETON                                        7199
036200011127     C                   leave
036300000000     C                   END
036400011127     C                   END
036500000000     C*--
036600000000     C* CAMPI MANUTENZIONE
036700000000     C*--
036800000000      * SE REGISTRO IL SINISTRO VINT DEVE ESSERE '  '
036900011127     C                   If        not *in98 and vint <> *BLANK
037000000000     C                   SETON                                        6299
037100011127     C                   leave
037200000000     C                   END
037300011127     c*
037400000000     C                   MOVEL     *BLANK        VINTD
037500011127     C                   if        not *in97
037600011128     c*controllo l'intervento solo se no sinistro
037700011127     C                   if        *in98
037800000000     C     VINT          IFEQ      *BLANKS
037900000000     C                   SETON                                        6399
038000011127     C                   leave
038100000000     C                   END
038200011127     c                   exsr      srvint
038300011128     C   99              leave
038400011128     C                   END
038500011128     C** DATA autorizzazione
038600000000     C     VDAT          IFEQ      0
038700000000     C                   SETON                                        7899
038800011127     C                   leave
038900000000     C                   ELSE
039000011127     C     *eur          test(d)                 vdat                   77
039100011127     c                   if        *in77
039200011127     C                   SETON                                        99
039300011127     C                   leave
039400000000     C                   END
039500000000     C                   END
039600011128     C** DATA fattura
039700000000     C     VDTF          IFNE      0
039800011127     C     *eur          test(d)                 vdtf                   85
039900011127     c                   if        *in85
040000011127     C                   SETON                                        99
040100011127     C                   leave
040200011127     C                   END
040300000000     C                   END
040400000000     C** OFFICINA
040500000000     C     VOFB          IFEQ      *BLANKS
040600000000     C                   SETON                                        8199
040700011127     C                   leave
040800000000     C                   END
040900011127     c                   exsr      srvofb
041000011128     c   99              leave
041100011128     C                   END
041200011127     C                   ENDdo
041300011127     C                   ENDSR
041400000000     C******************************************************
041500011127     c* ricerca
041600011127     C******************************************************
041700011127     C     ric01         BEGSR
041800011127     c                   movel     '1'           fld
041900011127     c                   select
042000011127     C*COD.autista
042100011127     C                   when      §fld = 'VAUS'
042200011127     c                   exsr      srvaus
042300011127     C*COD.interventi
042400011127     C                   when      §fld = 'VINT'
042500011127     c                   exsr      srvint
042600011127     C*COD.officina
042700011127     C                   when      §fld = 'VOFB'
042800011127     c                   exsr      srvofb
042900011127     C                   endsl
043000011127     C* POSIZIONAMENTO CURSORE
043100011127     C                   Z-ADD     §RIGA         §RIG
043200011127     C                   Z-ADD     §COLON        §COL
043300011127     C                   ENDSR
043400011127     c**********************************************************************
043500011127     C** autista
043600011127     c**********************************************************************
043700011127     C     srvaus        BEGSR
043800011127     c*
043900011127     c                   movel     'XN'          parcod
044000011127     c                   movel(P)  vaus          parkey
044100011127     c                   exsr      srtab
044200011127     c                   movel     parkey        vaus
044300011127     c                   if        *in99
044400011127     C                   MOVEL     *blanks       VDaus
044500011127     C                   SETON                                        76
044600011127     C                   else
044700011127     C                   MOVEL     TBLUNI        VDaus
044800011127     C                   END
044900011127     c*
045000011127     C                   ENDSR
045100011127     c**********************************************************************
045200011127     C** intervento
045300011127     c**********************************************************************
045400011127     C     srvint        BEGSR
045500011127     c*
045600011127     c                   movel     'XE'          parcod
045700011127     c                   movel(P)  vint          parkey
045800011127     c                   exsr      srtab
045900011127     c                   movel     parkey        vint
046000011127     c                   if        *in99
046100011127     C                   MOVEL     *blanks       Vintd
046200011127     C                   SETON                                        63
046300011127     C                   else
046400011127     C                   MOVEL     TBLUNI        Vintd
046500011127     C                   END
046600011127     c*
046700011127     C                   ENDSR
046800011127     c**********************************************************************
046900011127     C** officina
047000011127     c**********************************************************************
047100011127     C     srvofb        BEGSR
047200011127     c*
047300011127     c                   movel     'XO'          parcod
047400011127     c                   movel(P)  vofb          parkey
047500011127     c                   exsr      srtab
047600011127     c                   movel     parkey        vofb
047700011127     c                   if        *in99
047800011127     C                   MOVEL     *blanks       Vdofb
047900011127     C                   SETON                                        80
048000011127     C                   else
048100011127     C                   MOVEL     TBLUNI        Vdofb
048200011127     C                   END
048300011127     c*
048400011127     C                   ENDSR
048500011127     C**********************************************************************
048600011127     C** Controlli e decodifiche campi con tabel00f
048700011127     C**********************************************************************
048800011127     C     srtab         BEGSR
048900011127     c                   if        fld <> *blanks
049000011127     c                   movel     parkey        parkeys           8
049100011127     C                   CALL      'TRUL19R'
049200011127     c                   parm                    parcod            2
049300011127     c                   parm      '1'           parord            1
049400011127     c                   parm      *blanks       parkey            8
049500011127     c                   parm      *blanks       cmd               1
049600011127     c* ritorno senza scelta
049700011127     c                   if        cmd = 'L'
049800011127     c                   movel     parkeys       parkey
049900011127     C                   END
050000011127     C                   END
050100011127     C                   MOVEL     *blanks       tbluni
050200011127     C                   MOVEL     parcod        COD
050300011127     C                   MOVEL(p)  parkey        KEY
050400011128     C     KTAB          CHAIN     TABEL00f                           99
050500011127     c*
050600011127     C                   ENDSR
050700011127     C******************************************************
050800011127     C** AGGIORNAMENTI FILE
050900011127     C******************************************************
051000011127     C     AGGIOR        BEGSR
051100000000     C                   MOVE      VNRF          IGMNRF
051200000000     C                   MOVE      VINT          IGMINT
051300000000     C                   MOVE      VKM           IGMKM
051400011127     c                   do
051500011210     C** DELET  DI mzSIN E mzIGM
051600011127     C                   if        *in84
051700011210     C     parnsi        DELETE    mzSIN00F                           21
051800011203     C     parnig        DELETE    mzIGM00f                           21
051900011127     C                   leave
052000000000     C                   END
052100011210     C** SE I CAMPI DI mzIGM SONO TUTTI A 0 delete di mzsin e mzigm
052200000000     C** ALTRIMENTI UPDAT   SU TUTTI E 2
052300011127     C                   if        *in83
052400000000     C  N98              EXSR      AGGSIN
052500011210     C   98parnsi        DELETE    mzSIN00F                           21
052600000000     C  N97              EXSR      AGGIGM
052700011203     C   97parnig        DELETE    mzIGM00f                           21
052800000000     C                   END
052900011210     C** WRITE mzSIN/mzIGM
053000011127     C                   if        *in82
053100000000     C  N98              EXSR      AGGSIN
053200000000     C  N97              EXSR      AGGIGM
053300000000     C                   END
053400011127     C                   ENDdo
053500011127     C                   ENDSR
053600000000     C******************************************************
053700011210     C** mzSIN00F
053800011127     C******************************************************
053900000000     C     AGGSIN        BEGSR
054000000000     C* INCREMENTO N.SINISTRO
054100011203     C     parNSI        IFEQ      0
054200011219     C     *LOCK         IN        mzNSI
054300000000     C                   ADD       1             NUMSIN
054400011219     C                   OUT       mzNSI
054500000000     C                   Z-ADD     NUMSIN        SINNSI
054600000000     C                   END
054700000000     C**------------------------
054800011210     C     parnsi        CHAIN     mzSIN00F                           21
054900000000     C                   Z-ADD     VNRF          SINNRF
055000011127     C                   move      VDSI          Dtaeur
055100011127     C                   move      dtaeur        Dtaiso
055200011127     C                   MOVE      Dtaiso        SINDSI
055300000000     C                   MOVEL     VAUS          SINAUS
055400000000     C                   MOVEL     VATT          SINATT
055500000000     C                   MOVEL     VDE1          SINDE1
055600000000     C                   MOVEL     VDE2          SINDE2
055700000000     C                   MOVE      P91NOT        SINNOT
055800011210     C  N21              UPDATE    mzSIN000
055900011210     C   21              WRITE     mzSIN000
056000000000     C                   ENDSR
056100000000     C******************************************************
056200011128     C** mzIGM00F
056300011127     C******************************************************
056400000000     C     AGGIGM        BEGSR
056500011203     C     parnig        CHAIN     mzIGM00f                           21
056600000000     C                   Z-ADD     VNRF          IGMNRF
056700011127     C                   move      VDat          Dtaeur
056800011127     C                   move      dtaeur        Dtaiso
056900011127     C                   MOVE      Dtaiso        igmdat
057000000000     C                   Z-ADD     VKM           IGMKM
057100000000     C                   MOVE      VINT          IGMINT
057200000000     C     VDTF          IFNE      0
057300011127     C                   move      VDtf          Dtaeur
057400011127     C                   move      dtaeur        Dtaiso
057500011127     C                   MOVE      Dtaiso        igmdtf
057600000000     C                   ELSE
057700011128     C                   Z-ADD     0             IGMDTF
057800000000     C                   END
057900000000     C                   MOVEL     VINT          IGMINT
058000000000     C                   Z-ADD     0             IGMDIT
058100000000     C                   Z-ADD     0             IGMKCO
058200000000     C                   Z-ADD     0             IGMSCO
058300000000     C                   MOVEL     VOFF          IGMOFF
058400000000     C                   MOVEL     VOFB          IGMOFB
058500011128     C                   z-add     VOPV          IGMIPV
058600011128     C                   z-add     VOEF          IGMIEF
058700000000     C                   Z-ADD     SINNSI        IGMNSI
058800000000     C                   Z-ADD     P91NOT        IGMNOT
058900011128     C  N21              UPDATE    mzIGM000
059000011128     C   21              WRITE     mzIGM000
059100000000     C                   ENDSR
059200011127     C*----------------------------------------------------*
059300011127     c     *inzsr        begsr
059400011127     C*----------------------------------------------------*
059500011127     C* POSIZIONAMENTO TABELLE                             *
059600011127     C     KTAB          KLIST                                                  *
059700011127     C                   KFLD                    CODUT                          *
059800011127     C                   KFLD                    COD               2            *
059900011127     C                   KFLD                    KEY               8            *
060000160518     C     KTbe          KLIST                                                  *
060100160518     C                   KFLD                    tbecod                         *
060200160518     C                   KFLD                    tbeke1                         *
060300011210     C* mznot01L
060400011127     C     Knot          KLIST                                                  *
060500011127     C                   KFLD                    nottip                         *
060600011203     C                   KFLD                    P91not                         *
060700011203     C* mzigm03l
060800011203     C     Kigm          KLIST                                                  *
060900011203     C                   KFLD                    sinnrf                         *
061000011203     C                   KFLD                    SINnsi                         *
061100011127     C     *ENTRY        PLIST                                                  *
061200011127     C                   PARM                    KPJBA                          *
061300011127     C                   PARM                    TIPO             15            *
061400011127     C                   MOVE      1             CODUT
061500011127     C                   CALL      'XPARUT'
061600011127     C                   PARM                    PARUT
061700011127     C*--------- SE DITTA INESISTENTE FINE LAVORO          *
061800011127     C                   if        codut = -1
061900011127     c                   seton                                        lr
062000011127     c                   end
062100011127     C                   MOVEL     RAGUT         RSUT             20
062200011127     C                   MOVEL     KPJBU         PARAM
062300011128     c                   clear                   par191
062400011127     C** IMMISSIONE
062500011127     C     parmod        IFEQ      *BLANKS
062600011127     C                   SETON                                        82
062700011127     C                   END
062800011127     C** VARIAZIONE
062900011127     C     parmod        IFEQ      '2'
063000011127     C                   SETON                                        83
063100011127     C                   END
063200011127     C** ANNULLAMENTO
063300011127     C     parmod        IFEQ      '4'
063400011127     C                   SETON                                        8472
063500011127     C                   END
063600011221     C** VISUALIZZAZIONE
063700011221     C     parmod        IFEQ      '5'
063800011221     C                   SETON                                        73
063900011221     C                   END
064000011128     C** PROGRAMMA CHIAMANTE
064100011128     C     PARFLg        COMP      '1'                                    70
064200011219     C     *DTAARA       DEFINE                  mzNSI
06430000000005050C                   ENDSR
