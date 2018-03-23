000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200900606?     *--------------------------------------------------------------*
000300130225      * TNSC98R                                                      *
000400910916      *       *-----------------------------------*                  *
000500910916      *        INTERROGAZIONE STORICO CONTRASSEGNI                   *
000600910916      *       *-----------------------------------*                  *
000700910910      *
000800910521      * 12 -VIDEO PRECEDENTE                                         *
000900910910      * 15 -NESSUN RECORD DA CARICARE O FINE RECORD DA CARICARE      *
001000910910      * 16 -FINE RECORD VARIATI NEL SUBFILE                          *
001100910910      * 17 -INDICATORE DI COMODO                                     *
001200910910      * 30/36- ERRORI NEI CAMPI DELLA 1a VIDEATA                     *
001300910910      * 38 -ATTIVA MESSAGGIO DI PAGAMENTO BLOCCATO X QUEL CLIENTE    *
001400910910      * 40 -NON ATTIVA CARICAMENTO DI ALTRE SPEDIZIONI CON = NUM. DOC*
001500910910      *     SE NUMERO DOCUMENTO = 0                                  *
001600910910      * 50 -INDICATORE DI COMODO                                     *
001700910910      * 70 -ATTIVA DISATTIVA PULIZIA SUBFILE                         *
001800910917      * KL -FINE INTERROGAZIONE                                      *
001900900606?     *--------------------------------------------------------------*
002000130225     FTNSC98D   CF   E             WORKSTN
002100941026     FTNCSV01L  IF   E           K DISK
002200130307     FTNCSb03L  uF   E           K DISK
002300941026     FTNCSA01L  IF   E           K DISK
002400910909     FCNABI00F  IF   E           K DISK
002500941026     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
002600910906     FTABEL00F  IF   E           K DISK
002700941026?     *--------------------------------------------------------------*
002800980127     D TIBS69        E DS                  EXTNAME(TIBS69DS)
002900980127     D CNACO         E DS                  EXTNAME(CNACO00F)
003000980127     D CNIND         E DS                  EXTNAME(CNIND00F)
003100980127     D CNCLP         E DS                  EXTNAME(CNCLP00F)
003200980127     D FNCLS         E DS                  EXTNAME(FNCLS00F)
003300140804     D tncsvds       E DS                  EXTNAME(tncsv01l)
003400120223     d tnsc30ds      E DS
003500910708     D CNCR80        E DS
003600900514     D KPJBA         E DS
003700900516      *
003800900517     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003900000000     D  TCU                  398    697
004000000000     D                                     DIM(50)
004100000000     D  KCU                  698    847P 0
004200000000     D                                     DIM(50)
004300000000     D                                     PACKEVEN
004400900606      *
004500900606     D TCUDS           DS
004600900606     D  F3                     3      3
004700900606     D  F4                     4      4
004800900606     D  F56                    5      6
004900900515      *
005000941026     D WLBDA8          DS
005100941026     D  G02DAT                 1      8  0
005200941026     D  G02INV                 9     16  0
005300941026     D  G02ERR                17     17
005400941026     D  G02TGI                18     22  0
005500910521      *
005600950711     D TNSC94        E DS                  EXTNAME(TNSC94DS)
005700130306     d tibs55ds      E DS                  prefix(£)
005800130306     d param           ds
005900130306     D kaas                           4s 0
006000130306     D klnp                           3s 0
006100130306     D knrs                           2s 0
006200130306     D knsp                           7s 0
006300130306     D parca                          1
006400130306     D paresito                       1
006500130306     D sifda                         10
006600130306     D sifa                          10
006700941026?     *--------------------------------------------------------------*
006800941026     IAZORG01L  AA
006900941026     I                             P    4    5 0ORGFIL
007000941026     I                                 14   33  ORGDES
007100941026?    C*--------------------------------------------------------------*
007200130225      *
007300130308     c                   move      *year         vaas
007400130312     c                   clear                   aggiornato        1
007500130308     c                   do        *hival
007600130308     C     emetti        tag
007700130312     c                   if        aggiornato <> *blank
007800130312     c                   seton                                        28
007900130312     c                   eval      $msg = 'C/A aggiornato !!!'
008000130312     c                   clear                   aggiornato
008100130312     c                   endif
008200130226     C                   EXFMT     SC98d01
008300130226     c   kc              goto      fine
008400130226     C                   SETOFF                                       28
008500130226     c                   exsr      chkvideo1
008600130226     c   28              goto      emetti
008700910917     C                   EXSR      VISUAL
008800130226     c                   do        *hival
008900130308     c     sopra         tag
009000130226     C                   EXFMT     SC98D02
009100130308     c                   setoff                                       28
009200130226     c   kl              goto      emetti
009300130312     c                   exsr      chkvideo2
009400130312     c   28              goto      sopra
009500130307     c                   if        *inkf
009600130307     c                   exsr      conferma
009700130308     c  n28              leave
009800130307     c                   endif
009900130308     c* n28              goto      sopra
010000130306     c                   enddo
010100130308     c                   enddo
010200980127     C*
010300980127     C                   CLEAR                   TIBS69
010400980127     C                   MOVEL     'C'           I69TLA
010500980127     C                   CALL      'TIBS69R'
010600980127     C                   PARM                    TIBS69
010700980127     C                   PARM                    CNACO
010800980127     C                   PARM                    CNIND
010900980127     C                   PARM                    CNCLP
011000980127     C                   PARM                    FNCLS
011100980127     C*
011200130226     c     fine          tag
011300130306     C                   SETON                                        lr
011400130226      *-----------------------------------------------------*
011500130226     C     chkvideo1     BEGSR
011600130307      *-----------------------------------------------------*
011700130226     c                   if        vaas = 0 or
011800130226     c                             vlnp = 0 or
011900130226     c                             vnsp = 0
012000130226     c                   eval      $msg = 'Inserire i dati della -
012100130226     c                             spedizione completi'
012200130226     c                   seton                                        28
012300130226     c                   goto      endctr
012400130226     c                   endif
012500130226     C     Kspe          setgt     tncsv01l
012600130307     c                   do        *hival
012700130226     C     Kspe          readpe    tncsv01l
012800130226     c                   if        %eof(tncsv01l)
012900130226     c                   eval      $msg = 'Spedizione non trovata -
013000130226     c                             nello storico contrassegni'
013100130226     c                   seton                                        28
013200130226     c                   goto      endctr
013300130226     c                   endif
013400130226     c                   if        csvcav <> 'ERIN'
013500130307     c                   iter
013600130307     c                   else
013700130409     c                   if        csvtpi <> 'M'
013800130409     c                   eval      $msg = 'Assegno non intestat-
013900130409     c                             o al mittente non valido'
014000130409     c                   seton                                        28
014100130409     c                   goto      endctr
014200130409     c                   endif
014300140804     c                   clear                   tncsvds
014400140804
014500140804     c                   eval      d94aas = vaas
014600140804     c                   eval      d94lnp = vlnp
014700140804     c                   eval      d94nrs = vnrs
014800140804     c                   eval      d94nsp = vnsp
014900140804     c                   call      'TNSC98SR'
015000140804     c                   parm                    tnsc94
015100140804     c                   parm                    tncsvds
015200140807     c                   if        csvcav = *blank
015300140807     c                   eval      $msg = 'Non scelta nessuna -
015400140807     c                             variazione '
015500140807     c                   seton                                        28
015600140807     c                   goto      endctr
015700140807     c                   endif
015800130307     c                   leave
015900130226     c                   endif
016000130307     c                   enddo
016100130306     C                   movel     *blank        deslnp
016200130306     C     vlnp          chain     azORG01L                           50
016300130306     C                   MOVEL     ORGDES        DESLNP
016400130226     c     endctr        endsr
016500130306      *-----------------------------------------------------*
016600130306     C     chkvideo2     BEGSR
016700130306      *-----------------------------------------------------*
016800130307     C     Kspe          chain     tncsb03l
016900130307     c                   if        not %found(tncsb03l)
017000130306     c                   eval      $msg = 'Archivio Contrassegni -
017100130306     c                             non trovato -
017200130306     c                             aggiornamento non possibile'
017300130306     c                   seton                                        28
017400130306     c                   leavesr
017500130306     c                   endif
017600130307     c                   if        csbdta > 0 or
017700130306     c                             csbddp > 0 or
017800130306     c                             csbndp > 0 or
017900130306     c                             csbbna > 0
018000130306     c                   eval      $msg = 'Dati presenti aggior-
018100130306     c                             namento non possibile'
018200130306     c                   seton                                        28
018300130306     c                   leavesr
018400130306     c                   endif
018500130306     c                   if        csbsta <> 0
018600130306     c                   eval      $msg = 'Stato C/assegno non -
018700130306     c                             aggiornabile '
018800130306     c                   seton                                        28
018900130306     c                   leavesr
019000130306     c                   endif
019100130306     c                   endsr
019200130306      *-----------------------------------------------------*
019300130307     c     conferma      begsr
019400130306      *-----------------------------------------------------*
019500130307     c                   exsr      sraggfil
019600130307     c                   if        not *in28 or *inkg
019700130307     c                   eval      csbddc = csvddc
019800130307     c                   eval      csbndt = csvndt
019900130307     c                   eval      csbdta = csvdta
020000130307     c                   eval      csbnra = csvnra
020100130307     c                   eval      csbdte = csvdte
020200130307     c                   eval      csbtpa = csvtpa
020300130307     c                   eval      csbtpi = csvtpi
020400130307     c                   eval      csbabi = csvabi
020500130307     c                   eval      csbcab = csvcab
020600130307     c                   eval      csbddp = csvddp
020700130307     c                   eval      csbndp = csvndp
020800130307     c                   eval      csbbna = csvbna
020900130315     c                   eval      csbksc = csvksc
021000130315     c                   eval      csbcdi = csvcdi
021100130315     c                   eval      csbcas = csvcas
021200130315     c                   eval      csbvca = csvvca
021300130315     c                   eval      csbcmb = csvcmb
021400130306     c                   update    tncsb000
021500130312     c                   move      'X'           aggiornato
021600130306     c                   endif
021700130306     c                   endsr
021800130306     C*-------------------------------------------------------*
021900130306     C* Aggiorna gli archivi gestionali
022000130306     C*-------------------------------------------------------*
022100130306     C     sraggfil      BEGSR
022200130306     c                   setoff                                       45
022300130306     C* Richiamo TIBS55R per conoscere S.Informativi
022400130306     C* in cui immettere il lavoro
022500130306     C                   CLEAR                   TIBS55ds
022600130306     C                   MOVEL     'L'           £I50TLA
022700130306     C                   MOVEL     '046'         £I50PPO
022800130306     C                   MOVEL     '001'         £I50APO
022900130306     C                   CALL      'TIBS55R'
023000130306     C                   PARM                    TIBS55ds
023100130306     C* MI SALVO IL S.I. ORIGINALE XCHÈ SE NO NON LO REIMPOSTA
023200130306     c                   clear                   param
023300130306     c                   eval      sifda = KNSIF
023400130306     c                   eval      sifa = £O50ASI
023500130306     c                   eval      parca= 'C'
023600130311     c                   eval      kaas = csvaas
023700130311     c                   eval      klnp = csvlnp
023800130311     c                   eval      knrs = csvnrs
023900130311     c                   eval      knsp = csvnsp
024000130306     C                   MOVEL(p)  PARAM         KPJBU
024100130306     c                   call      'CNC0W13C'
024200130306     C                   PARM                    KPJBA
024300130306     c                   movel     kpjbu         param
024400130306     c                   if        paresito = '1'
024500130307     c                   seton                                        2831
024600130307     c                   eval      $msg = 'Aggiornamento filiale-
024700130307     c                              non avvenuto F7 x Forzare -
024800130307     c                              aggiornamento C/assegno'
024900130306     c                   seton                                        28
025000130307     c                   exfmt     sc98d02
025100130307     c                   setoff                                       2831
025200130306     c                   leavesr
025300130306     c                   end
025400130306     C*
025500130306     C                   ENDSR
025600941026      *-----------------------------------------------------*
025700130226      *   CARICAMENTO video 2                               *
025800941026      *-----------------------------------------------------*
025900910828     C     VISUAL        BEGSR
026000910917     C                   Z-ADD     CSVSTA        RSTA
026100910917     C                   MOVEL     '4S'          CODTAB
026200910917     C                   MOVE      RSTA          KEYTAB
026300910917     C     KTAB          CHAIN     TABEL                              50
026400910917     C  N50              MOVEL     TBLUNI        DESSTA
026500910917     C   50              MOVEL     *BLANKS       DESSTA
026600910917     C* FILIALE VARIAZIONE
026700910917     C                   MOVEL     CSVFEV        RFEV
026800910917     C* CAUSALE VARIAZIONE
026900910917     C                   MOVEL     CSVCAV        RCAV
027000910917     C* DATA VARIAZIONE
027100941026     C                   Z-ADD     CSVDTV        G02INV
027200941026     C                   Z-ADD     0             G02DAT
027300941026     C                   MOVEL     '3'           G02ERR
027400941026     C                   CALL      'XSRDA8'
027500941026     C                   PARM                    WLBDA8
027600941026     C                   Z-ADD     G02DAT        RDTV
027700910917     C* LINEA DI PARTENZA + DECODIFICA
027800120224     C                   Z-ADD     CSVorv        horv
027900120224     C                   Z-ADD     CSVdtv        hdtv
028000120223     C                   Z-ADD     CSVAAS        RAAS
028100120223     C                   Z-ADD     CSVLNP        RLNP
028200910917     C* NR° SERIE + SPEDIZIONE
028300910917     C                   Z-ADD     CSVNRS        RNRS
028400910917     C                   Z-ADD     CSVNSP        RNSP
028500910917     C* CODICE CLIENTE
028600910917     C                   Z-ADD     CSVKSC        RKSC
028700910917     C* DATA SPEDIZIONE
028800941026     C                   MOVE      CSVMGS        G02INV
028900941026     C                   MOVEL     CSVAAS        G02INV
029000941026     C                   Z-ADD     0             G02DAT
029100941026     C                   MOVEL     '3'           G02ERR
029200941026     C                   CALL      'XSRDA8'
029300941026     C                   PARM                    WLBDA8
029400941026     C                   Z-ADD     G02DAT        RDSP
029500910917     C* DATA DISTINTA
029600941026     C                   Z-ADD     CSVDDC        G02INV
029700941026     C                   Z-ADD     0             G02DAT
029800941026     C                   MOVEL     '3'           G02ERR
029900941026     C                   CALL      'XSRDA8'
030000941026     C                   PARM                    WLBDA8
030100941026     C                   Z-ADD     G02DAT        RDDC
030200910917     C* LINEA ARRIVO + DECODIFICA
030300910917     C                   Z-ADD     CSVLNA        RLNA
030400941026     C                   Z-ADD     CSVLNA        ORGFIL
030500941026     C                   MOVEL     *BLANKS       ORGDES
030600941026     C     ORGFIL        CHAIN     AZORG01L                           50
030700941026     C                   MOVEL     ORGDES        DESLNA
030800910917     C* NUMERO DISTINTA
030900910917     C                   Z-ADD     CSVNDT        RNDT
031000910917     C* RIFERIMENTO MITTENTE
031100910917     C                   Z-ADD     CSVRMN        RRMN
031200941026     C                   MOVEL     CSVRMA        RRMA
031300910917     C* DATA ARRIVO DISTINTA
031400941026     C                   Z-ADD     CSVDTA        G02INV
031500941026     C                   Z-ADD     0             G02DAT
031600941026     C                   MOVEL     '3'           G02ERR
031700941026     C                   CALL      'XSRDA8'
031800941026     C                   PARM                    WLBDA8
031900941026     C                   Z-ADD     G02DAT        RDTA
032000910917     C* NR° ASSEGNO = RIFERIM. INCASSO
032100910917     C                   MOVEL     CSVNRA        RNRA
032200120223     c                   if        %subst(csvnra: 10: 1) <> ' '
032300120223     c                             and csvabi = 0
032400120223     c                   seton                                        09
032500120223     c                   else
032600120223     c                   setoff                                       09
032700120223     c                   end
032800910917     C* DATA ASSEGNO
032900941026     C                   Z-ADD     CSVDTE        G02INV
033000941026     C                   Z-ADD     0             G02DAT
033100941026     C                   MOVEL     '3'           G02ERR
033200941026     C                   CALL      'XSRDA8'
033300941026     C                   PARM                    WLBDA8
033400941026     C                   Z-ADD     G02DAT        RDTE
033500910917     C* TIPO INTESTAZIONE ASSEGNO
033600910917     C                   MOVEL     CSVTPI        RTPI
033700941026     C                   MOVEL     CSVTPA        RTPA
033800910917     C* CODICE INTESTAZIONE ASS.
033900910917     C                   Z-ADD     CSVCDI        RCDI
034000910917     C* ABI / CAB / EVENTUALE DECODIFICA BANCA
034100910917     C                   Z-ADD     CSVABI        RABI
034200910917     C                   Z-ADD     CSVCAB        RCAB
034300941026     C                   MOVEL     *BLANKS       ABIIST
034400910917     C     KABI          CHAIN     CNABI000                           75
034500941026     C                   MOVEL     ABIIST        RDES
034600941026     C* DATA REGISTRAZIONE INCASSO
034700941026     C                   Z-ADD     CSVDRI        G02INV
034800941026     C                   Z-ADD     0             G02DAT
034900941026     C                   MOVEL     '3'           G02ERR
035000941026     C                   CALL      'XSRDA8'
035100941026     C                   PARM                    WLBDA8
035200941026     C                   Z-ADD     G02DAT        RDRI
035300941026     C* NUMERO REG.INCASSO/CAMBIO
035400941026     C                   Z-ADD     CSVNRI        RNRI
035500941026     C                   Z-ADD     CSVCMB        RCMB
035600910917     C* INTESTAZIONE ASSEGNO / MITTENTE
035700910917     C* SE COD. INTESTAZ. ASSEGNO <> COD. CLIENTE LO DECODIFICO
035800910917     C     RCDI          IFNE      RKSC
035900980127      * CHIAMO TIBS69 PER RICERCA CODICE CLIENTE CODIFICATO
036000980127     C                   CLEAR                   TIBS69
036100980127     C                   CLEAR                   CNACO
036200980127     C                   CLEAR                   CNIND
036300980127     C                   CLEAR                   CNCLP
036400980127     C                   CLEAR                   FNCLS
036500980127     C                   MOVEL     ' '           I69TLA
036600980127     C                   MOVEL     KNSIF         I69SIF
036700980127     C                   Z-ADD     RCDI          I69KAC
036800980127     C                   Z-ADD     RCDI          I69KIN
036900980127     C                   CALL      'TIBS69R'
037000980127     C                   PARM                    TIBS69
037100980127     C                   PARM                    CNACO
037200980127     C                   PARM                    CNIND
037300980127     C                   PARM                    CNCLP
037400980127     C                   PARM                    FNCLS
037500980127     C     O69ERR        IFEQ      *BLANK
037600910917     C                   MOVEL     ACORAG        RINT
037700910917     C                   MOVEL     INDVIA        VIAINT
037800910917     C                   MOVEL     INDCIT        CITINT
037900941028     C                   MOVEL     *BLANKS       CAPINT
038000941028     C     INDCAP        IFNE      *ZEROS
038100941028     C                   MOVEL     INDCAP        CAPINT
038200941028     C                   END
038300941026     C                   MOVEL     INDSTA        NAZINT
038400941026     C                   MOVEL     INDPRV        PROINT
038500941026     C     INDCAE        IFNE      *ZEROS
038600941026     C     INDCAE        ANDNE     *BLANKS
038700941026     C                   MOVEL     INDCAE        CAPINT
038800941026     C                   END
038900910917     C                   END
039000980127     C                   END
039100910917     C* SE CLIENTE VARIO LO DECODIFICO DA BLCSA
039200910917     C                   MOVE      RKSC          APP               4
039300910917     C     APP           IFEQ      '8888'
039400910917     C     APP           OREQ      '9999'
039500941026     C     KBLCAS        CHAIN     TNCSA000                           50
039600910917     C                   MOVEL     CSARSD        DESMIO
039700910917     C                   MOVEL     CSAVID        INDMIO
039800941026     C                   MOVEL     CSACAD        CAPMIO
039900941026     C                   MOVEL     CSACID        LOCMIO
040000941026     C                   MOVEL     CSANZM        NAZMIO
040100941026     C                   MOVEL     CSAPRM        PROMIO
040200910917     C                   ELSE
040300980127      * CHIAMO TIBS69 PER RICERCA CODICE CLIENTE CODIFICATO
040400980127     C                   CLEAR                   TIBS69
040500980127     C                   CLEAR                   CNACO
040600980127     C                   CLEAR                   CNIND
040700980127     C                   CLEAR                   CNCLP
040800980127     C                   CLEAR                   FNCLS
040900980127     C                   MOVEL     ' '           I69TLA
041000980127     C                   MOVEL     KNSIF         I69SIF
041100980127     C                   Z-ADD     RKSC          I69KAC
041200980127     C                   Z-ADD     RKSC          I69KIN
041300980127     C                   CALL      'TIBS69R'
041400980127     C                   PARM                    TIBS69
041500980127     C                   PARM                    CNACO
041600980127     C                   PARM                    CNIND
041700980127     C                   PARM                    CNCLP
041800980127     C                   PARM                    FNCLS
041900980127     C     O69ERR        IFEQ      *BLANK
042000910917     C                   MOVEL     ACORAG        DESMIO
042100910917     C                   MOVEL     INDVIA        INDMIO
042200910917     C                   MOVEL     INDCIT        LOCMIO
042300941028     C                   MOVEL     *BLANKS       CAPMIO
042400941028     C     INDCAP        IFNE      *ZEROS
042500941028     C                   MOVEL     INDCAP        CAPMIO
042600941028     C                   END
042700941026     C                   MOVEL     INDSTA        NAZMIO
042800941026     C                   MOVEL     INDPRV        PROMIO
042900941026     C     INDCAE        IFNE      *ZEROS
043000941026     C     INDCAE        ANDNE     *BLANKS
043100941026     C                   MOVEL     INDCAE        CAPMIO
043200941026     C                   END
043300980127     C                   END
043400910917     C                   END
043500910917     C* DESTINATARIO
043600910917     C                   MOVEL     CSVRSD        RRSD
043700910917     C                   MOVEL     CSVPAR        RPAR
043800910917     C                   MOVEL     CSVPRO        RPRO
043900910917     C* NR° DOCUMENTO PAGAMENTO
044000910917     C                   Z-ADD     CSVNDP        RNDP
044100910917     C* BANCA PAGAMENTO - DECODIFICA SOLO SE COD. <> 0
044200910917     C                   MOVEL     *BLANKS       RBAN
044300910917     C                   Z-ADD     CSVBNA        RBNA
044400910917     C     CSVBNA        IFNE      0
044500980127     C                   CLEAR                   TIBS69
044600980127     C                   CLEAR                   CNACO
044700980127     C                   CLEAR                   CNIND
044800980127     C                   CLEAR                   CNCLP
044900980127     C                   CLEAR                   FNCLS
045000980127     C                   MOVEL     ' '           I69TLA
045100980127     C                   MOVEL     KNSIF         I69SIF
045200980127     C                   Z-ADD     KBNA          I69KCC
045300980127     C                   Z-ADD     CSVBNA        I69KAC
045400980127     C                   MOVEL     *BLANKS       ACORAG
045500980127     C                   CALL      'TIBS69R'
045600980127     C                   PARM                    TIBS69
045700980127     C                   PARM                    CNACO
045800980127     C                   PARM                    CNIND
045900980127     C                   PARM                    CNCLP
046000980127     C                   PARM                    FNCLS
046100980127     C     O69ERR        IFEQ      *BLANK
046200980127     C                   MOVEL     ACORAG        RBAN
046300910917     C                   END
046400980127     C                   END
046500910917     C* DATA PAGAMENTO
046600941026     C                   Z-ADD     CSVDDP        G02INV
046700941026     C                   Z-ADD     0             G02DAT
046800941026     C                   MOVEL     '3'           G02ERR
046900941026     C                   CALL      'XSRDA8'
047000941026     C                   PARM                    WLBDA8
047100941026     C                   Z-ADD     G02DAT        RDDP
047200941026     C* DATA REGISTRAZIONE PAGAMENTO
047300941026     C                   Z-ADD     CSVDRG        G02INV
047400941026     C                   Z-ADD     0             G02DAT
047500941026     C                   MOVEL     '3'           G02ERR
047600941026     C                   CALL      'XSRDA8'
047700941026     C                   PARM                    WLBDA8
047800941026     C                   Z-ADD     G02DAT        RDRG
047900941026     C                   Z-ADD     CSVNRG        RNRG
048000941026     C                   Z-ADD     CSVCMP        RCMP
048100910917     C* TIPO PAGAMENTO EFFETTUATO
048200910917     C                   MOVEL     CSVFPC        RFPC
048300910917     C* DATI EVENTUALE GIACENZA
048400910917     C                   Z-ADD     CSVNGC        RNGC
048500941026     C                   Z-ADD     CSVDGC        G02INV
048600941026     C                   Z-ADD     0             G02DAT
048700941026     C                   MOVEL     '3'           G02ERR
048800941026     C                   CALL      'XSRDA8'
048900941026     C                   PARM                    WLBDA8
049000941026     C                   Z-ADD     G02DAT        RDGC
049100910917     C* RAGGRUPPAMENTO CONTABILE
049200910917     C                   Z-ADD     CSVRGF        RRGF
049300910917     C* TIPO BOLLA
049400910917     C                   MOVEL     CSVTBL        RTBL
049500910917     C* IMPORTO C/A
049600941026     C                   Z-ADD     CSVCAS        RCAS
049700941028     C                   MOVEL     CSVVCA        RVCA
049800941028     C                   MOVEL     CSVGCA        RGCA
049900941028     C*
050000941028     C     PROMIO        COMP      *BLANKS                            10
050100941028     C     NAZMIO        COMP      *BLANKS                            11
050200941028     C     PROINT        COMP      *BLANKS                            14
050300941028     C     NAZINT        COMP      *BLANKS                            13
050400910906     C*
050500910902     C                   ENDSR
050600941026      *---------------------------------------------------------------*
050700941026      *  OPERAZIONI INIZIALI
050800941026      *---------------------------------------------------------------*
050900941026     C     *INZSR        BEGSR
051000941026     C*
051100941026     C     *ENTRY        PLIST
051200941026     C                   PARM                    KPJBA
051300950711     C                   MOVEL     KPJBU         TNSC94
051400941026      *
051500941026      * CHIAVI
051600941026     C     KTAB          KLIST
051700941026     C                   KFLD                    CODUT
051800941026     C                   KFLD                    CODTAB            2
051900941026     C                   KFLD                    KEYTAB            8
052000941026      *
052100941026     C     KSPE          KLIST
052200130225     C                   KFLD                    vAAS
052300130225     C                   KFLD                    vLNP
052400130225     C                   KFLD                    vNRS
052500130225     C                   KFLD                    vNSP
052600941026     C*
052700941026     C     KBLCAS        KLIST
052800941026     C                   KFLD                    RAAS
052900941026     C                   KFLD                    RLNP
053000941026     C                   KFLD                    RNRS
053100941026     C                   KFLD                    RNSP
053200941026     C*
053300941026     C     KBAN1         KLIST
053400941026     C                   KFLD                    CODUT
053500941026     C                   KFLD                    KBNA
053600941026     C                   KFLD                    RBNA
053700941026     C*
053800941026     C     KABI          KLIST
053900941026     C                   KFLD                    CSVABI
054000941026     C                   KFLD                    CSVCAB
054100941026      *---------------------------------------------------------------*
054200980127     C                   Z-ADD     1             CODUT             1 0
054300941026     C                   CALL      'X§PARUT'
054400941026     C                   PARM                    UT§DSE
054500941026     C*          CODUT     CABEQ-1        FINE
054600941026     C                   MOVEL     RAGUT         RSUT             20
054700941026     C                   MOVEL     REC80         CNCR80
054800941026     C                   MOVEL     KNMUS         §SEI              6
054900941026     C                   CLEAR                   WLBDA8
055000941026      *
055100941026      *  RICERCA CAPOCONTI
055200941026     C                   DO        50            X                 2 0
055300941026     C                   MOVE      TCU(X)        TCUDS
055400980127     C     F56           IFEQ      'CG'
055500941026     C     F4            IFEQ      '6'
055600941026     C                   Z-ADD     KCU(X)        KBNA              4 0
055700941026     C                   END
055800941026     C                   END
055900941026     C*
056000941026     C                   END
056100941026     C*
056200941026     C                   ENDSR
