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
039000170906      ***
039100170906      **  controlla quanti caratteri sono stati inseriti
039200170906      **    x aggiungere o l'anno o il secolo.
039300170906     c                   z-add     vdat          dataGEN           8 0
039400170906     c                   exsr      imposta_data
039500170906     c                   z-add     dataGEN       vdat
039600170906      ***
039700011127     C     *eur          test(d)                 vdat                   77
039800011127     c                   if        *in77
039900011127     C                   SETON                                        99
040000011127     C                   leave
040100000000     C                   END
040200000000     C                   END
040300011128     C** DATA fattura
040400000000     C     VDTF          IFNE      0
040500170906      ***
040600170906      **  controlla quanti caratteri sono stati inseriti
040700170906      **    x aggiungere o l'anno o il secolo.
040800170906     c                   z-add     vdtf          dataGEN
040900170906     c                   exsr      imposta_data
041000170906     c                   z-add     dataGEN       vdtf
041100170906      ***
041200011127     C     *eur          test(d)                 vdtf                   85
041300011127     c                   if        *in85
041400011127     C                   SETON                                        99
041500011127     C                   leave
041600011127     C                   END
041700000000     C                   END
041800000000     C** OFFICINA
041900000000     C     VOFB          IFEQ      *BLANKS
042000000000     C                   SETON                                        8199
042100011127     C                   leave
042200000000     C                   END
042300011127     c                   exsr      srvofb
042400011128     c   99              leave
042500011128     C                   END
042600011127     C                   ENDdo
042700011127     C                   ENDSR
042800000000     C******************************************************
042900170906     c*   completa la data se inserita parzialmente
043000011127     C******************************************************
043100170906     C     imposta_data  BEGSR
043200170906      *
043300170906     c                   clear                   lungo             3 0
043400170906     c                   movel     dataGEN       campo4            4
043500170906     c                   if        campo4 ='0000'
043600170906     c                   eval      lungo = 4
043700170906     c                   else
043800170906     c                   if        %subst(campo4:1:2) ='00'
043900170906     c                   eval      lungo = 6
044000170906     c                   end
044100170906     c                   end
044200170906     c                   if        lungo = 4
044300170906     c                   eval      dataGEN = dataGEN * 10000 + *year
044400170906     c                   elseif    lungo = 6
044500170906     c                   move      dataGEN       anno2             2 0
044600170906     c                   movel     *year         anno4             4 0
044700170906     c                   move      anno2         anno4
044800170906     c                   move      dataGEN       data6             6 0
044900170906     c                   move      '00'          data6
045000170906     c                   eval      dataGEN = data6 * 100 + anno4
045100170906     c                   end
045200170906      *
045300170906     C                   ENDSR
045400170906     C******************************************************
045500170906     c* ricerca
045600170906     C******************************************************
045700170906     C     ric01         BEGSR
045800011127     c                   movel     '1'           fld
045900011127     c                   select
046000011127     C*COD.autista
046100011127     C                   when      §fld = 'VAUS'
046200011127     c                   exsr      srvaus
046300011127     C*COD.interventi
046400011127     C                   when      §fld = 'VINT'
046500011127     c                   exsr      srvint
046600011127     C*COD.officina
046700011127     C                   when      §fld = 'VOFB'
046800011127     c                   exsr      srvofb
046900011127     C                   endsl
047000011127     C* POSIZIONAMENTO CURSORE
047100011127     C                   Z-ADD     §RIGA         §RIG
047200011127     C                   Z-ADD     §COLON        §COL
047300011127     C                   ENDSR
047400011127     c**********************************************************************
047500011127     C** autista
047600011127     c**********************************************************************
047700011127     C     srvaus        BEGSR
047800011127     c*
047900011127     c                   movel     'XN'          parcod
048000011127     c                   movel(P)  vaus          parkey
048100011127     c                   exsr      srtab
048200011127     c                   movel     parkey        vaus
048300011127     c                   if        *in99
048400011127     C                   MOVEL     *blanks       VDaus
048500011127     C                   SETON                                        76
048600011127     C                   else
048700011127     C                   MOVEL     TBLUNI        VDaus
048800011127     C                   END
048900011127     c*
049000011127     C                   ENDSR
049100011127     c**********************************************************************
049200011127     C** intervento
049300011127     c**********************************************************************
049400011127     C     srvint        BEGSR
049500011127     c*
049600011127     c                   movel     'XE'          parcod
049700011127     c                   movel(P)  vint          parkey
049800011127     c                   exsr      srtab
049900011127     c                   movel     parkey        vint
050000011127     c                   if        *in99
050100011127     C                   MOVEL     *blanks       Vintd
050200011127     C                   SETON                                        63
050300011127     C                   else
050400011127     C                   MOVEL     TBLUNI        Vintd
050500011127     C                   END
050600011127     c*
050700011127     C                   ENDSR
050800011127     c**********************************************************************
050900011127     C** officina
051000011127     c**********************************************************************
051100011127     C     srvofb        BEGSR
051200011127     c*
051300011127     c                   movel     'XO'          parcod
051400011127     c                   movel(P)  vofb          parkey
051500011127     c                   exsr      srtab
051600011127     c                   movel     parkey        vofb
051700011127     c                   if        *in99
051800011127     C                   MOVEL     *blanks       Vdofb
051900011127     C                   SETON                                        80
052000011127     C                   else
052100011127     C                   MOVEL     TBLUNI        Vdofb
052200011127     C                   END
052300011127     c*
052400011127     C                   ENDSR
052500011127     C**********************************************************************
052600011127     C** Controlli e decodifiche campi con tabel00f
052700011127     C**********************************************************************
052800011127     C     srtab         BEGSR
052900011127     c                   if        fld <> *blanks
053000011127     c                   movel     parkey        parkeys           8
053100011127     C                   CALL      'TRUL19R'
053200011127     c                   parm                    parcod            2
053300011127     c                   parm      '1'           parord            1
053400011127     c                   parm      *blanks       parkey            8
053500011127     c                   parm      *blanks       cmd               1
053600011127     c* ritorno senza scelta
053700011127     c                   if        cmd = 'L'
053800011127     c                   movel     parkeys       parkey
053900011127     C                   END
054000011127     C                   END
054100011127     C                   MOVEL     *blanks       tbluni
054200011127     C                   MOVEL     parcod        COD
054300011127     C                   MOVEL(p)  parkey        KEY
054400011128     C     KTAB          CHAIN     TABEL00f                           99
054500011127     c*
054600011127     C                   ENDSR
054700011127     C******************************************************
054800011127     C** AGGIORNAMENTI FILE
054900011127     C******************************************************
055000011127     C     AGGIOR        BEGSR
055100000000     C                   MOVE      VNRF          IGMNRF
055200000000     C                   MOVE      VINT          IGMINT
055300000000     C                   MOVE      VKM           IGMKM
055400011127     c                   do
055500011210     C** DELET  DI mzSIN E mzIGM
055600011127     C                   if        *in84
055700011210     C     parnsi        DELETE    mzSIN00F                           21
055800011203     C     parnig        DELETE    mzIGM00f                           21
055900011127     C                   leave
056000000000     C                   END
056100011210     C** SE I CAMPI DI mzIGM SONO TUTTI A 0 delete di mzsin e mzigm
056200000000     C** ALTRIMENTI UPDAT   SU TUTTI E 2
056300011127     C                   if        *in83
056400000000     C  N98              EXSR      AGGSIN
056500011210     C   98parnsi        DELETE    mzSIN00F                           21
056600000000     C  N97              EXSR      AGGIGM
056700011203     C   97parnig        DELETE    mzIGM00f                           21
056800000000     C                   END
056900011210     C** WRITE mzSIN/mzIGM
057000011127     C                   if        *in82
057100000000     C  N98              EXSR      AGGSIN
057200000000     C  N97              EXSR      AGGIGM
057300000000     C                   END
057400011127     C                   ENDdo
057500011127     C                   ENDSR
057600000000     C******************************************************
057700011210     C** mzSIN00F
057800011127     C******************************************************
057900000000     C     AGGSIN        BEGSR
058000000000     C* INCREMENTO N.SINISTRO
058100011203     C     parNSI        IFEQ      0
058200011219     C     *LOCK         IN        mzNSI
058300000000     C                   ADD       1             NUMSIN
058400011219     C                   OUT       mzNSI
058500000000     C                   Z-ADD     NUMSIN        SINNSI
058600000000     C                   END
058700000000     C**------------------------
058800011210     C     parnsi        CHAIN     mzSIN00F                           21
058900000000     C                   Z-ADD     VNRF          SINNRF
059000011127     C                   move      VDSI          Dtaeur
059100011127     C                   move      dtaeur        Dtaiso
059200011127     C                   MOVE      Dtaiso        SINDSI
059300000000     C                   MOVEL     VAUS          SINAUS
059400000000     C                   MOVEL     VATT          SINATT
059500000000     C                   MOVEL     VDE1          SINDE1
059600000000     C                   MOVEL     VDE2          SINDE2
059700000000     C                   MOVE      P91NOT        SINNOT
059800011210     C  N21              UPDATE    mzSIN000
059900011210     C   21              WRITE     mzSIN000
060000000000     C                   ENDSR
060100000000     C******************************************************
060200011128     C** mzIGM00F
060300011127     C******************************************************
060400000000     C     AGGIGM        BEGSR
060500011203     C     parnig        CHAIN     mzIGM00f                           21
060600000000     C                   Z-ADD     VNRF          IGMNRF
060700011127     C                   move      VDat          Dtaeur
060800011127     C                   move      dtaeur        Dtaiso
060900011127     C                   MOVE      Dtaiso        igmdat
061000000000     C                   Z-ADD     VKM           IGMKM
061100000000     C                   MOVE      VINT          IGMINT
061200000000     C     VDTF          IFNE      0
061300011127     C                   move      VDtf          Dtaeur
061400011127     C                   move      dtaeur        Dtaiso
061500011127     C                   MOVE      Dtaiso        igmdtf
061600000000     C                   ELSE
061700011128     C                   Z-ADD     0             IGMDTF
061800000000     C                   END
061900000000     C                   MOVEL     VINT          IGMINT
062000000000     C                   Z-ADD     0             IGMDIT
062100000000     C                   Z-ADD     0             IGMKCO
062200000000     C                   Z-ADD     0             IGMSCO
062300000000     C                   MOVEL     VOFF          IGMOFF
062400000000     C                   MOVEL     VOFB          IGMOFB
062500011128     C                   z-add     VOPV          IGMIPV
062600011128     C                   z-add     VOEF          IGMIEF
062700000000     C                   Z-ADD     SINNSI        IGMNSI
062800000000     C                   Z-ADD     P91NOT        IGMNOT
062900011128     C  N21              UPDATE    mzIGM000
063000011128     C   21              WRITE     mzIGM000
063100000000     C                   ENDSR
063200011127     C*----------------------------------------------------*
063300011127     c     *inzsr        begsr
063400011127     C*----------------------------------------------------*
063500011127     C* POSIZIONAMENTO TABELLE                             *
063600011127     C     KTAB          KLIST                                                  *
063700011127     C                   KFLD                    CODUT                          *
063800011127     C                   KFLD                    COD               2            *
063900011127     C                   KFLD                    KEY               8            *
064000160518     C     KTbe          KLIST                                                  *
064100160518     C                   KFLD                    tbecod                         *
064200160518     C                   KFLD                    tbeke1                         *
064300011210     C* mznot01L
064400011127     C     Knot          KLIST                                                  *
064500011127     C                   KFLD                    nottip                         *
064600011203     C                   KFLD                    P91not                         *
064700011203     C* mzigm03l
064800011203     C     Kigm          KLIST                                                  *
064900011203     C                   KFLD                    sinnrf                         *
065000011203     C                   KFLD                    SINnsi                         *
065100011127     C     *ENTRY        PLIST                                                  *
065200011127     C                   PARM                    KPJBA                          *
065300011127     C                   PARM                    TIPO             15            *
065400011127     C                   MOVE      1             CODUT
065500011127     C                   CALL      'XPARUT'
065600011127     C                   PARM                    PARUT
065700011127     C*--------- SE DITTA INESISTENTE FINE LAVORO          *
065800011127     C                   if        codut = -1
065900011127     c                   seton                                        lr
066000011127     c                   end
066100011127     C                   MOVEL     RAGUT         RSUT             20
066200011127     C                   MOVEL     KPJBU         PARAM
066300011128     c                   clear                   par191
066400011127     C** IMMISSIONE
066500011127     C     parmod        IFEQ      *BLANKS
066600011127     C                   SETON                                        82
066700011127     C                   END
066800011127     C** VARIAZIONE
066900011127     C     parmod        IFEQ      '2'
067000011127     C                   SETON                                        83
067100011127     C                   END
067200011127     C** ANNULLAMENTO
067300011127     C     parmod        IFEQ      '4'
067400011127     C                   SETON                                        8472
067500011127     C                   END
067600011221     C** VISUALIZZAZIONE
067700011221     C     parmod        IFEQ      '5'
067800011221     C                   SETON                                        73
067900011221     C                   END
068000011128     C** PROGRAMMA CHIAMANTE
068100011128     C     PARFLg        COMP      '1'                                    70
068200011219     C     *DTAARA       DEFINE                  mzNSI
06830000000005050C                   ENDSR
