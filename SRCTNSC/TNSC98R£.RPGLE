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
002300941026     FTNCSV01L  IF   E           K DISK
002301130307     FTNCSb03L  uF   E           K DISK
002600941026     FTNCSA01L  IF   E           K DISK
002900910909     FCNABI00F  IF   E           K DISK
003000941026     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
003100910906     FTABEL00F  IF   E           K DISK
003200941026?     *--------------------------------------------------------------*
003300980127     D TIBS69        E DS                  EXTNAME(TIBS69DS)
003400980127     D CNACO         E DS                  EXTNAME(CNACO00F)
003500980127     D CNIND         E DS                  EXTNAME(CNIND00F)
003600980127     D CNCLP         E DS                  EXTNAME(CNCLP00F)
003700980127     D FNCLS         E DS                  EXTNAME(FNCLS00F)
003800120223     d tnsc30ds      E DS
003900910708     D CNCR80        E DS
004000900514     D KPJBA         E DS
004300900516      *
004700900517     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
004800000000     D  TCU                  398    697
004900000000     D                                     DIM(50)
005000000000     D  KCU                  698    847P 0
005100000000     D                                     DIM(50)
005200000000     D                                     PACKEVEN
005300900606      *
005400900606     D TCUDS           DS
005500900606     D  F3                     3      3
005600900606     D  F4                     4      4
005700900606     D  F56                    5      6
005800900515      *
005900941026     D WLBDA8          DS
006000941026     D  G02DAT                 1      8  0
006100941026     D  G02INV                 9     16  0
006200941026     D  G02ERR                17     17
006300941026     D  G02TGI                18     22  0
006400910521      *
006500950711     D TNSC94        E DS                  EXTNAME(TNSC94DS)
006501130306     d tibs55ds      E DS                  prefix(£)
006502130306     d param           ds
006503130306     D kaas                           4s 0
006504130306     D klnp                           3s 0
006505130306     D knrs                           2s 0
006506130306     D knsp                           7s 0
006507130306     D parca                          1
006508130306     D paresito                       1
006509130306     D sifda                         10
006510130306     D sifa                          10
006600941026?     *--------------------------------------------------------------*
006700941026     IAZORG01L  AA
006800941026     I                             P    4    5 0ORGFIL
006900941026     I                                 14   33  ORGDES
007000941026?    C*--------------------------------------------------------------*
007205130225      *
007206130308     c                   move      *year         vaas
007207130312     c                   clear                   aggiornato        1
007701130308     c                   do        *hival
007702130308     C     emetti        tag
007703130312     c                   if        aggiornato <> *blank
007705130312     c                   seton                                        28
007706130312     c                   eval      $msg = 'C/A aggiornato !!!'
007707130312     c                   clear                   aggiornato
007708130312     c                   endif
007800130226     C                   EXFMT     SC98d01
007801130226     c   kc              goto      fine
007900130226     C                   SETOFF                                       28
007901130226     c                   exsr      chkvideo1
007902130226     c   28              goto      emetti
008300910917     C                   EXSR      VISUAL
008301130226     c                   do        *hival
008302130308     c     sopra         tag
008400130226     C                   EXFMT     SC98D02
008401130308     c                   setoff                                       28
008402130226     c   kl              goto      emetti
008403130312     c                   exsr      chkvideo2
008404130312     c   28              goto      sopra
008405130307     c                   if        *inkf
008406130307     c                   exsr      conferma
008407130308     c  n28              leave
008408130307     c                   endif
008409130308     c* n28              goto      sopra
008410130306     c                   enddo
008411130308     c                   enddo
010600980127     C*
010700980127     C                   CLEAR                   TIBS69
010800980127     C                   MOVEL     'C'           I69TLA
010900980127     C                   CALL      'TIBS69R'
011000980127     C                   PARM                    TIBS69
011100980127     C                   PARM                    CNACO
011200980127     C                   PARM                    CNIND
011300980127     C                   PARM                    CNCLP
011400980127     C                   PARM                    FNCLS
011500980127     C*
011501130226     c     fine          tag
011600130306     C                   SETON                                        lr
011611130226      *-----------------------------------------------------*
011612130226     C     chkvideo1     BEGSR
011613130307      *-----------------------------------------------------*
011614130226     c                   if        vaas = 0 or
011615130226     c                             vlnp = 0 or
011616130226     c                             vnsp = 0
011617130226     c                   eval      $msg = 'Inserire i dati della -
011618130226     c                             spedizione completi'
011619130226     c                   seton                                        28
011620130226     c                   goto      endctr
011621130226     c                   endif
011622130226     C     Kspe          setgt     tncsv01l
011623130307     c                   do        *hival
011624130226     C     Kspe          readpe    tncsv01l
011625130226     c                   if        %eof(tncsv01l)
011626130226     c                   eval      $msg = 'Spedizione non trovata -
011627130226     c                             nello storico contrassegni'
011628130226     c                   seton                                        28
011629130226     c                   goto      endctr
011630130226     c                   endif
011631130226     c                   if        csvcav <> 'ERIN'
011632130307     c                   iter
011633130307     c                   else
011634130409     c                   if        csvtpi <> 'M'
011635130409     c                   eval      $msg = 'Assegno non intestat-
011636130409     c                             o al mittente non valido'
011637130409     c                   seton                                        28
011638130409     c                   goto      endctr
011639130409     c                   endif
011640130307     c                   leave
011641130226     c                   endif
011644130307     c                   enddo
011645130306     C                   movel     *blank        deslnp
011646130306     C     vlnp          chain     azORG01L                           50
011647130306     C                   MOVEL     ORGDES        DESLNP
011648130226     c     endctr        endsr
011649130306      *-----------------------------------------------------*
011650130306     C     chkvideo2     BEGSR
011651130306      *-----------------------------------------------------*
011652130307     C     Kspe          chain     tncsb03l
011653130307     c                   if        not %found(tncsb03l)
011654130306     c                   eval      $msg = 'Archivio Contrassegni -
011655130306     c                             non trovato -
011656130306     c                             aggiornamento non possibile'
011657130306     c                   seton                                        28
011658130306     c                   leavesr
011659130306     c                   endif
011661130307     c                   if        csbdta > 0 or
011668130306     c                             csbddp > 0 or
011669130306     c                             csbndp > 0 or
011670130306     c                             csbbna > 0
011671130306     c                   eval      $msg = 'Dati presenti aggior-
011672130306     c                             namento non possibile'
011673130306     c                   seton                                        28
011674130306     c                   leavesr
011675130306     c                   endif
011676130306     c                   if        csbsta <> 0
011677130306     c                   eval      $msg = 'Stato C/assegno non -
011678130306     c                             aggiornabile '
011679130306     c                   seton                                        28
011680130306     c                   leavesr
011681130306     c                   endif
011688130306     c                   endsr
011689130306      *-----------------------------------------------------*
011690130307     c     conferma      begsr
011691130306      *-----------------------------------------------------*
011692130307     c                   exsr      sraggfil
011693130307     c                   if        not *in28 or *inkg
011694130307     c                   eval      csbddc = csvddc
011695130307     c                   eval      csbndt = csvndt
011696130307     c                   eval      csbdta = csvdta
011697130307     c                   eval      csbnra = csvnra
011698130307     c                   eval      csbdte = csvdte
011699130307     c                   eval      csbtpa = csvtpa
011700130307     c                   eval      csbtpi = csvtpi
011701130307     c                   eval      csbabi = csvabi
011702130307     c                   eval      csbcab = csvcab
011703130307     c                   eval      csbddp = csvddp
011704130307     c                   eval      csbndp = csvndp
011705130307     c                   eval      csbbna = csvbna
011706130315     c                   eval      csbksc = csvksc
011707130315     c                   eval      csbcdi = csvcdi
011708130315     c                   eval      csbcas = csvcas
011709130315     c                   eval      csbvca = csvvca
011710130315     c                   eval      csbcmb = csvcmb
011711130306     c                   update    tncsb000
011712130312     c                   move      'X'           aggiornato
011713130306     c                   endif
011714130306     c                   endsr
011715130306     C*-------------------------------------------------------*
011716130306     C* Aggiorna gli archivi gestionali
011717130306     C*-------------------------------------------------------*
011718130306     C     sraggfil      BEGSR
011719130306     c                   setoff                                       45
011720130306     C* Richiamo TIBS55R per conoscere S.Informativi
011721130306     C* in cui immettere il lavoro
011722130306     C                   CLEAR                   TIBS55ds
011723130306     C                   MOVEL     'L'           £I50TLA
011724130306     C                   MOVEL     '046'         £I50PPO
011725130306     C                   MOVEL     '001'         £I50APO
011726130306     C                   CALL      'TIBS55R'
011727130306     C                   PARM                    TIBS55ds
011728130306     C* MI SALVO IL S.I. ORIGINALE XCHÈ SE NO NON LO REIMPOSTA
011729130306     c                   clear                   param
011730130306     c                   eval      sifda = KNSIF
011731130306     c                   eval      sifa = £O50ASI
011732130306     c                   eval      parca= 'C'
011733130311     c                   eval      kaas = csvaas
011734130311     c                   eval      klnp = csvlnp
011735130311     c                   eval      knrs = csvnrs
011736130311     c                   eval      knsp = csvnsp
011737130306     C                   MOVEL(p)  PARAM         KPJBU
011738130306     c                   call      'CNC0W13C'
011739130306     C                   PARM                    KPJBA
011740130306     c                   movel     kpjbu         param
011741130306     c                   if        paresito = '1'
011742130307     c                   seton                                        2831
011743130307     c                   eval      $msg = 'Aggiornamento filiale-
011744130307     c                              non avvenuto F7 x Forzare -
011745130307     c                              aggiornamento C/assegno'
011746130306     c                   seton                                        28
011747130307     c                   exfmt     sc98d02
011748130307     c                   setoff                                       2831
011749130306     c                   leavesr
011750130306     c                   end
011751130306     C*
011752130306     C                   ENDSR
019700941026      *-----------------------------------------------------*
019800130226      *   CARICAMENTO video 2                               *
019900941026      *-----------------------------------------------------*
020000910828     C     VISUAL        BEGSR
020300910917     C                   Z-ADD     CSVSTA        RSTA
020400910917     C                   MOVEL     '4S'          CODTAB
020500910917     C                   MOVE      RSTA          KEYTAB
020600910917     C     KTAB          CHAIN     TABEL                              50
020700910917     C  N50              MOVEL     TBLUNI        DESSTA
020800910917     C   50              MOVEL     *BLANKS       DESSTA
020900910917     C* FILIALE VARIAZIONE
021000910917     C                   MOVEL     CSVFEV        RFEV
021100910917     C* CAUSALE VARIAZIONE
021200910917     C                   MOVEL     CSVCAV        RCAV
021300910917     C* DATA VARIAZIONE
021400941026     C                   Z-ADD     CSVDTV        G02INV
021500941026     C                   Z-ADD     0             G02DAT
021600941026     C                   MOVEL     '3'           G02ERR
021700941026     C                   CALL      'XSRDA8'
021800941026     C                   PARM                    WLBDA8
021900941026     C                   Z-ADD     G02DAT        RDTV
022000910917     C* LINEA DI PARTENZA + DECODIFICA
022100120224     C                   Z-ADD     CSVorv        horv
022200120224     C                   Z-ADD     CSVdtv        hdtv
022300120223     C                   Z-ADD     CSVAAS        RAAS
022400120223     C                   Z-ADD     CSVLNP        RLNP
022900910917     C* NR° SERIE + SPEDIZIONE
023000910917     C                   Z-ADD     CSVNRS        RNRS
023100910917     C                   Z-ADD     CSVNSP        RNSP
023200910917     C* CODICE CLIENTE
023300910917     C                   Z-ADD     CSVKSC        RKSC
023400910917     C* DATA SPEDIZIONE
023500941026     C                   MOVE      CSVMGS        G02INV
023600941026     C                   MOVEL     CSVAAS        G02INV
023700941026     C                   Z-ADD     0             G02DAT
023800941026     C                   MOVEL     '3'           G02ERR
023900941026     C                   CALL      'XSRDA8'
024000941026     C                   PARM                    WLBDA8
024100941026     C                   Z-ADD     G02DAT        RDSP
024200910917     C* DATA DISTINTA
024300941026     C                   Z-ADD     CSVDDC        G02INV
024400941026     C                   Z-ADD     0             G02DAT
024500941026     C                   MOVEL     '3'           G02ERR
024600941026     C                   CALL      'XSRDA8'
024700941026     C                   PARM                    WLBDA8
024800941026     C                   Z-ADD     G02DAT        RDDC
024900910917     C* LINEA ARRIVO + DECODIFICA
025000910917     C                   Z-ADD     CSVLNA        RLNA
025100941026     C                   Z-ADD     CSVLNA        ORGFIL
025200941026     C                   MOVEL     *BLANKS       ORGDES
025300941026     C     ORGFIL        CHAIN     AZORG01L                           50
025400941026     C                   MOVEL     ORGDES        DESLNA
025500910917     C* NUMERO DISTINTA
025600910917     C                   Z-ADD     CSVNDT        RNDT
025700910917     C* RIFERIMENTO MITTENTE
025800910917     C                   Z-ADD     CSVRMN        RRMN
025900941026     C                   MOVEL     CSVRMA        RRMA
026000910917     C* DATA ARRIVO DISTINTA
026100941026     C                   Z-ADD     CSVDTA        G02INV
026200941026     C                   Z-ADD     0             G02DAT
026300941026     C                   MOVEL     '3'           G02ERR
026400941026     C                   CALL      'XSRDA8'
026500941026     C                   PARM                    WLBDA8
026600941026     C                   Z-ADD     G02DAT        RDTA
026700910917     C* NR° ASSEGNO = RIFERIM. INCASSO
026800910917     C                   MOVEL     CSVNRA        RNRA
026900120223     c                   if        %subst(csvnra: 10: 1) <> ' '
027000120223     c                             and csvabi = 0
027100120223     c                   seton                                        09
027200120223     c                   else
027300120223     c                   setoff                                       09
027400120223     c                   end
027500910917     C* DATA ASSEGNO
027600941026     C                   Z-ADD     CSVDTE        G02INV
027700941026     C                   Z-ADD     0             G02DAT
027800941026     C                   MOVEL     '3'           G02ERR
027900941026     C                   CALL      'XSRDA8'
028000941026     C                   PARM                    WLBDA8
028100941026     C                   Z-ADD     G02DAT        RDTE
028200910917     C* TIPO INTESTAZIONE ASSEGNO
028300910917     C                   MOVEL     CSVTPI        RTPI
028400941026     C                   MOVEL     CSVTPA        RTPA
028500910917     C* CODICE INTESTAZIONE ASS.
028600910917     C                   Z-ADD     CSVCDI        RCDI
028700910917     C* ABI / CAB / EVENTUALE DECODIFICA BANCA
028800910917     C                   Z-ADD     CSVABI        RABI
028900910917     C                   Z-ADD     CSVCAB        RCAB
029000941026     C                   MOVEL     *BLANKS       ABIIST
029100910917     C     KABI          CHAIN     CNABI000                           75
029200941026     C                   MOVEL     ABIIST        RDES
029300941026     C* DATA REGISTRAZIONE INCASSO
029400941026     C                   Z-ADD     CSVDRI        G02INV
029500941026     C                   Z-ADD     0             G02DAT
029600941026     C                   MOVEL     '3'           G02ERR
029700941026     C                   CALL      'XSRDA8'
029800941026     C                   PARM                    WLBDA8
029900941026     C                   Z-ADD     G02DAT        RDRI
030000941026     C* NUMERO REG.INCASSO/CAMBIO
030100941026     C                   Z-ADD     CSVNRI        RNRI
030200941026     C                   Z-ADD     CSVCMB        RCMB
030300910917     C* INTESTAZIONE ASSEGNO / MITTENTE
030400910917     C* SE COD. INTESTAZ. ASSEGNO <> COD. CLIENTE LO DECODIFICO
030500910917     C     RCDI          IFNE      RKSC
030800980127      * CHIAMO TIBS69 PER RICERCA CODICE CLIENTE CODIFICATO
030900980127     C                   CLEAR                   TIBS69
031000980127     C                   CLEAR                   CNACO
031100980127     C                   CLEAR                   CNIND
031200980127     C                   CLEAR                   CNCLP
031300980127     C                   CLEAR                   FNCLS
031400980127     C                   MOVEL     ' '           I69TLA
031500980127     C                   MOVEL     KNSIF         I69SIF
031600980127     C                   Z-ADD     RCDI          I69KAC
031700980127     C                   Z-ADD     RCDI          I69KIN
031800980127     C                   CALL      'TIBS69R'
031900980127     C                   PARM                    TIBS69
032000980127     C                   PARM                    CNACO
032100980127     C                   PARM                    CNIND
032200980127     C                   PARM                    CNCLP
032300980127     C                   PARM                    FNCLS
032400980127     C     O69ERR        IFEQ      *BLANK
032500910917     C                   MOVEL     ACORAG        RINT
032600910917     C                   MOVEL     INDVIA        VIAINT
032700910917     C                   MOVEL     INDCIT        CITINT
032800941028     C                   MOVEL     *BLANKS       CAPINT
032900941028     C     INDCAP        IFNE      *ZEROS
033000941028     C                   MOVEL     INDCAP        CAPINT
033100941028     C                   END
033200941026     C                   MOVEL     INDSTA        NAZINT
033300941026     C                   MOVEL     INDPRV        PROINT
033400941026     C     INDCAE        IFNE      *ZEROS
033500941026     C     INDCAE        ANDNE     *BLANKS
033600941026     C                   MOVEL     INDCAE        CAPINT
033700941026     C                   END
033800910917     C                   END
033900980127     C                   END
034000910917     C* SE CLIENTE VARIO LO DECODIFICO DA BLCSA
034100910917     C                   MOVE      RKSC          APP               4
034200910917     C     APP           IFEQ      '8888'
034300910917     C     APP           OREQ      '9999'
034400941026     C     KBLCAS        CHAIN     TNCSA000                           50
034500910917     C                   MOVEL     CSARSD        DESMIO
034600910917     C                   MOVEL     CSAVID        INDMIO
034700941026     C                   MOVEL     CSACAD        CAPMIO
034800941026     C                   MOVEL     CSACID        LOCMIO
034900941026     C                   MOVEL     CSANZM        NAZMIO
035000941026     C                   MOVEL     CSAPRM        PROMIO
035100910917     C                   ELSE
035200980127      * CHIAMO TIBS69 PER RICERCA CODICE CLIENTE CODIFICATO
035300980127     C                   CLEAR                   TIBS69
035400980127     C                   CLEAR                   CNACO
035500980127     C                   CLEAR                   CNIND
035600980127     C                   CLEAR                   CNCLP
035700980127     C                   CLEAR                   FNCLS
035800980127     C                   MOVEL     ' '           I69TLA
035900980127     C                   MOVEL     KNSIF         I69SIF
036000980127     C                   Z-ADD     RKSC          I69KAC
036100980127     C                   Z-ADD     RKSC          I69KIN
036200980127     C                   CALL      'TIBS69R'
036300980127     C                   PARM                    TIBS69
036400980127     C                   PARM                    CNACO
036500980127     C                   PARM                    CNIND
036600980127     C                   PARM                    CNCLP
036700980127     C                   PARM                    FNCLS
036800980127     C     O69ERR        IFEQ      *BLANK
037100910917     C                   MOVEL     ACORAG        DESMIO
037200910917     C                   MOVEL     INDVIA        INDMIO
037300910917     C                   MOVEL     INDCIT        LOCMIO
037400941028     C                   MOVEL     *BLANKS       CAPMIO
037500941028     C     INDCAP        IFNE      *ZEROS
037600941028     C                   MOVEL     INDCAP        CAPMIO
037700941028     C                   END
037800941026     C                   MOVEL     INDSTA        NAZMIO
037900941026     C                   MOVEL     INDPRV        PROMIO
038000941026     C     INDCAE        IFNE      *ZEROS
038100941026     C     INDCAE        ANDNE     *BLANKS
038200941026     C                   MOVEL     INDCAE        CAPMIO
038300941026     C                   END
038400980127     C                   END
038500910917     C                   END
038600910917     C* DESTINATARIO
038700910917     C                   MOVEL     CSVRSD        RRSD
038800910917     C                   MOVEL     CSVPAR        RPAR
038900910917     C                   MOVEL     CSVPRO        RPRO
039000910917     C* NR° DOCUMENTO PAGAMENTO
039100910917     C                   Z-ADD     CSVNDP        RNDP
039200910917     C* BANCA PAGAMENTO - DECODIFICA SOLO SE COD. <> 0
039300910917     C                   MOVEL     *BLANKS       RBAN
039400910917     C                   Z-ADD     CSVBNA        RBNA
039500910917     C     CSVBNA        IFNE      0
039600980127     C                   CLEAR                   TIBS69
039700980127     C                   CLEAR                   CNACO
039800980127     C                   CLEAR                   CNIND
039900980127     C                   CLEAR                   CNCLP
040000980127     C                   CLEAR                   FNCLS
040100980127     C                   MOVEL     ' '           I69TLA
040200980127     C                   MOVEL     KNSIF         I69SIF
040300980127     C                   Z-ADD     KBNA          I69KCC
040400980127     C                   Z-ADD     CSVBNA        I69KAC
040500980127     C                   MOVEL     *BLANKS       ACORAG
040600980127     C                   CALL      'TIBS69R'
040700980127     C                   PARM                    TIBS69
040800980127     C                   PARM                    CNACO
040900980127     C                   PARM                    CNIND
041000980127     C                   PARM                    CNCLP
041100980127     C                   PARM                    FNCLS
041200980127     C     O69ERR        IFEQ      *BLANK
041400980127     C                   MOVEL     ACORAG        RBAN
041500910917     C                   END
041600980127     C                   END
041700910917     C* DATA PAGAMENTO
041800941026     C                   Z-ADD     CSVDDP        G02INV
041900941026     C                   Z-ADD     0             G02DAT
042000941026     C                   MOVEL     '3'           G02ERR
042100941026     C                   CALL      'XSRDA8'
042200941026     C                   PARM                    WLBDA8
042300941026     C                   Z-ADD     G02DAT        RDDP
042400941026     C* DATA REGISTRAZIONE PAGAMENTO
042500941026     C                   Z-ADD     CSVDRG        G02INV
042600941026     C                   Z-ADD     0             G02DAT
042700941026     C                   MOVEL     '3'           G02ERR
042800941026     C                   CALL      'XSRDA8'
042900941026     C                   PARM                    WLBDA8
043000941026     C                   Z-ADD     G02DAT        RDRG
043100941026     C                   Z-ADD     CSVNRG        RNRG
043200941026     C                   Z-ADD     CSVCMP        RCMP
043300910917     C* TIPO PAGAMENTO EFFETTUATO
043400910917     C                   MOVEL     CSVFPC        RFPC
043500910917     C* DATI EVENTUALE GIACENZA
043600910917     C                   Z-ADD     CSVNGC        RNGC
043700941026     C                   Z-ADD     CSVDGC        G02INV
043800941026     C                   Z-ADD     0             G02DAT
043900941026     C                   MOVEL     '3'           G02ERR
044000941026     C                   CALL      'XSRDA8'
044100941026     C                   PARM                    WLBDA8
044200941026     C                   Z-ADD     G02DAT        RDGC
044300910917     C* RAGGRUPPAMENTO CONTABILE
044400910917     C                   Z-ADD     CSVRGF        RRGF
044500910917     C* TIPO BOLLA
044600910917     C                   MOVEL     CSVTBL        RTBL
044700910917     C* IMPORTO C/A
044800941026     C                   Z-ADD     CSVCAS        RCAS
044900941028     C                   MOVEL     CSVVCA        RVCA
045000941028     C                   MOVEL     CSVGCA        RGCA
045100941028     C*
045200941028     C     PROMIO        COMP      *BLANKS                            10
045300941028     C     NAZMIO        COMP      *BLANKS                            11
045400941028     C     PROINT        COMP      *BLANKS                            14
045500941028     C     NAZINT        COMP      *BLANKS                            13
045600910906     C*
045700910902     C                   ENDSR
045800941026      *---------------------------------------------------------------*
045900941026      *  OPERAZIONI INIZIALI
046000941026      *---------------------------------------------------------------*
046100941026     C     *INZSR        BEGSR
046200941026     C*
046300941026     C     *ENTRY        PLIST
046400941026     C                   PARM                    KPJBA
046500950711     C                   MOVEL     KPJBU         TNSC94
046600941026      *
046700941026      * CHIAVI
046800941026     C     KTAB          KLIST
046900941026     C                   KFLD                    CODUT
047000941026     C                   KFLD                    CODTAB            2
047100941026     C                   KFLD                    KEYTAB            8
047200941026      *
047300941026     C     KSPE          KLIST
047400130225     C                   KFLD                    vAAS
047500130225     C                   KFLD                    vLNP
047600130225     C                   KFLD                    vNRS
047700130225     C                   KFLD                    vNSP
047800941026     C*
047900941026     C     KBLCAS        KLIST
048000941026     C                   KFLD                    RAAS
048100941026     C                   KFLD                    RLNP
048200941026     C                   KFLD                    RNRS
048300941026     C                   KFLD                    RNSP
048400941026     C*
048500941026     C     KBAN1         KLIST
048600941026     C                   KFLD                    CODUT
048700941026     C                   KFLD                    KBNA
048800941026     C                   KFLD                    RBNA
048900941026     C*
049000941026     C     KABI          KLIST
049100941026     C                   KFLD                    CSVABI
049200941026     C                   KFLD                    CSVCAB
049300941026      *---------------------------------------------------------------*
049400980127     C                   Z-ADD     1             CODUT             1 0
049500941026     C                   CALL      'X§PARUT'
049600941026     C                   PARM                    UT§DSE
049700941026     C*          CODUT     CABEQ-1        FINE
049800941026     C                   MOVEL     RAGUT         RSUT             20
049900941026     C                   MOVEL     REC80         CNCR80
050000941026     C                   MOVEL     KNMUS         §SEI              6
050100941026     C                   CLEAR                   WLBDA8
050200941026      *
050300941026      *  RICERCA CAPOCONTI
050400941026     C                   DO        50            X                 2 0
050500941026     C                   MOVE      TCU(X)        TCUDS
050600980127     C     F56           IFEQ      'CG'
050700941026     C     F4            IFEQ      '6'
050800941026     C                   Z-ADD     KCU(X)        KBNA              4 0
050900941026     C                   END
051000941026     C                   END
051100941026     C*
051200941026     C                   END
051300941026     C*
051400941026     C                   ENDSR
