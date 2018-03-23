000100020322     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200020322      *------------------------------------------------------------------------*
000300020322      * STAMPA BOLLE PER PUNTO OPERATIVO UNITA' EDP - ESEGUE                   *
000400020322      *------------------------------------------------------------------------*
000500131118     FTABEL00F  IF   E           K DISK
000600020528     FAZORG21l  IF   E           K DISK
000700020528     fazorg01l  if   e           k disk
000800941114     FUNMOV02L  UF   E           K DISK
000900941114     FUNANA01L  IF   E           K DISK
001000100915     FUNgio00F  uF   E             DISK    RENAME(UNgio000:UNgioFIS)
001100100915     FUNgio01L  IF   E           K DISK    RENAME(UNgio000:UNgio1) infds(gio01)
001200100916     FUNgio03L  uF   E           K DISK    RENAME(UNgio000:UNgio3)
001300100915     f
001400131213     FFIVAX50F  O  A E           K DISK
001500131203     FFIVABWKF  O  A E           K DISK    USROPN
001600131118     FFIVAB50F  O  A E             DISK    RENAME(FIVAB000:FIVAB050)
001700101008     FLET132    O    F  132        PRINTER OFLIND(*INOF)
001800140207     FFNLV22P   O    F  132        PRINTER
001900020322
002000920505     D TES             S             45    DIM(2) CTDATA PERRCD(1)
002100140207     D GAI             S             13    DIM(3) CTDATA PERRCD(1)
002200100915     d
002300100915     D gio01           DS
002400100915     D  gio1NRR              397    400B 0
002500020528
002600020528     d wappoggio       s              1
002700100924     d s_orgd70        s                   like(orgd70)
002800100924     d s_orgdit        s                   like(orgdit)
002900101008     d wmag            s                   like(unamag)
003000131212     d wtvax           s              1    inz
003100131213     d prg             s              4  0 inz
003200020528
003300041201     d trul33ds      e ds
003400131118     d
003500941114     D KPJBA         E DS
003600941114     D PARAM           DS
003700941114     D  PARMAG                 1      3  0
003800941114     D  PARDAT                 4     11  0
003900941114     D  PARRIS                12     12
004000100910     D  PARncl                13     17  0
004100100910     D  PARpkg                18     24  1
004200100910     D  PARMAUT               25     25
004300100915     D  PARcuf                26     28
004400101028     D  PARAUM                29     29
004500131203     D  PARbol                30     30
004600131203     d* - parametri da TRUL90R:
004700131203     d* · pgm.   per stampa LDV
004800131203     d  wD90psl              157    166
004900131203     d* · stampante per LDV in formato "A4" (laser)
005000131203     d  wD90prb4             167    176
005100131203     d* · stampante per LDV in formato "A5" (laser)
005200131203     d  wD90prb5             177    186
005300131203     d* · modulo per stampa LDV su "A4"     (laser)
005400131203     d  wD90mdb4             187    196
005500131203     d* · modulo per stampa LDV su "A5"     (laser)
005600131203     d  wD90mdb5             197    206
005700131203     d* · pgm di stampa etichette segnacolli
005800131203     d  wD90pss              207    216
005900131203     d* · modulo per stampa segnacolli
006000131203     d  wD90mde              227    236
006100131203     d* · stampante per segnacolli
006200131203     d  wD90pre              247    256
006300131203     d
006400100910     d
006500941114     D CNCR40        E DS
006600120322     D CNCR42        E DS
006700941114     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
006800910610     D                 DS
006900020920
007000131203     D DS5E          E DS
007100131118     d ds3a          e ds
007200131118     d og143         e ds
007300100930     d og148         e ds
007400131118     D DSLV55        E DS                  EXTNAME(FNLV55DS)
007500131118     D DSSI95        E DS                  EXTNAME(TISI95DS)
007600131118     D DSLV13        E DS                  EXTNAME(FNLV13DS)
007700131203     D DSBLD         E DS
007800131203     D* STAMPANTE SEGNACOLLI
007900131203     D* DS ESTERA PER  STAMPA BOLLE
008000131203     d Fnlsb5ds      e ds
008100131203     d
008200131203     D DSBLDE        E DS                  INZ
008300080924     d
008400080924     D WLBDAT          DS                  INZ
008500080924     D  G02DAT                 1      8  0
008600080924     D  G02INV                 9     16  0
008700080924     D  G02ERR                17     17
008800080924     D  G02TGI                18     22  0
008900131203     D DSACO         E DS                  EXTNAME(CNACO00F)
009000131203     D DSIND         E DS                  EXTNAME(CNIND00F)
009100131203     D DSCLP         E DS                  EXTNAME(CNCLP00F)
009200131203     D DSCLS         E DS                  EXTNAME(FNCLS00F)
009300131203     D DSBS69        E DS                  EXTNAME(TIBS69DS)
009400120322     d
009500120322     d  ragio218       s             30
009600120322     d  loc_218        s             30
009700120322     d  ind_218        s             30
009800120322     d  prv_218        s              2
009900120322     d  cap_218        s              5  0
010000120322     d  wfil           s              3  0
010100140409     d  wntr           s                   like(vablna)
010200020322
010300941115     C*------------------------------------------------------------------------*
010400941115     C* INDICATORI
010500941115     C*------------------------------------------------------------------------*
010600941115     C* 10 - CAUSALE NON PRESENTE
010700941115     C* 11 - MAGAZZINO INESISTENTE
010800941115     C* 12 - QUANTITA' OBBLIGATORIA
010900941115     C* 13 - MAGAZZINO CORRELATO INESISTENTE
011000941115     C* 14 - DATA NON VALIDA
011100941115     C* 16 - ON SE C'E' CAUSALE CORRELATA
011200941114     C*------------------------------------------------------------------------*
011300941114     C* MAIN LINE
011400941114     C*------------------------------------------------------------------------*
011500941114     C* RICEVIMENTO PARAMETRI
011600941114     C     *ENTRY        PLIST
011700941114     C                   PARM                    KPJBA
011800941114     C                   Z-ADD     1             CODUT
011900941114     C                   CALL      'X§PARUT'
012000941114     C                   PARM                    UTEDSE
012100941114     C                   MOVEL     REC40         CNCR40
012200120322     C                   MOVEL     REC42         CNCR42
012300120322     C                   MOVEL     simrgs        RAGIO            30
012400120322     C*****              MOVEL     'SETRAS  '    RAGIO            30
012500941115     C                   MOVEL     *ALL'-'       TRA              80
012600120322     C
012700120322     C* USO FILIALE 218 PER L'INDIRIZZO DI SETRAS
012800120322     C                   EVAL      WFIL=218
012900120322     C     wfil          CHAIN     AZORG21l
013000120322     c                   if        %found(azorg21l)
013100120322     c                   eval      ragio218=orgdes
013200120322     c                   eval      ind_218=orgind
013300120322     c                   eval      loc_218=orgloc
013400120322     c                   eval      prv_218=orgpro
013500120322     c                   eval      cap_218=orgcpf
013600120322     c                   endif
013700941114     C*
013800960105     C* CONTROLLO SE IL SISTEMA INFORMATIVO  E' RIFERITO A GAITRA201
013900941114     C     KNSIF         IFEQ      GAI(1)
014000100920     C     KNSIF         oreq      GAI(3)
014100021030     C                   OPEN      FIVABWKF
014200921102     C                   Z-ADD     1             FLGAI             1 0
014300941114     C                   END
014400080924     c* Prendo la data del giorno
014500080924     C**
014600080924     C                   TIME                    W0140            14 0
014700080924     C                   MOVE      W0140         WDTGIO            8 0
014800080924     C*
014900080924     C                   Z-ADD     WDTGIO        G02DAT
015000080924     C                   MOVEL     *BLANK        G02ERR
015100080924     C                   CALL      'XSRDA8'
015200080924     C                   PARM                    WLBDAT
015300080924     C                   MOVEL     G02INV        DATEU             8 0
015400900116     C*
015500941114     C* LETTURA FLNUF01L - COMPLETA
015600041201     C**   KNUF          KLIST
015700041201     C**                 KFLD                    YEAR              2 0
015800041201     C**                 KFLD                    CODN              3 0
015900941114     C*
016000941114     C* LETTURA UNMOV02L - COMPLETA
016100920505     C     KIAVE         KLIST
016200941114     C                   KFLD                    PARMAG
016300971013     C                   KFLD                    PARDAT
016400941114     C*
016500941114     C* LETTURA UNANA01L - COMPLETA
016600941114     C     KANA          KLIST
016700941114     C                   KFLD                    UNMTIP
016800941114     C                   KFLD                    UNMCOD
016900100915     C* LETTURA UNgio01l -
017000100915     C     Kungio        KLIST
017100100915     C                   KFLD                    UNMTIP
017200110407     C                   KFLD                    unmmat
017300100915     C                   KFLD                    parcuf
017400100916     C* LETTURA UNgio03l -
017500100916     C     Kungio3       KLIST
017600100916     C                   KFLD                    parcuf
017700100916     C                   KFLD                    pardat
017800100916     C                   KFLD                    parmag
017900131118     C     KTAB2         KLIST
018000131118     C                   KFLD                    CODUT
018100131118     C                   KFLD                    COD               2
018200131118     C                   KFLD                    KEY               8
018300941114     C*
018400920505     C                   MOVEL     KPJBU         PARAM
018500960105     C* 15 ON - RISTAMPA
018600941114     C     PARRIS        COMP      'S'                                    15
018700920505     C* PRELEVO DATI FILIALE
018800020528     C                   clear                   wappoggio
018900020924     c                   Clear                   og143
019000100930     c                   Clear                   og148
019100020531     C     PARMAG        CHAIN     AZORG21l
019200110613     c                   if        %found(azorg21l) and orgfag<>' '
019300020528     c                   eval      wappoggio = 'S'
019400140409     c* memorizzo eventuale lna presente in orgntr
019500140409     c                   move      orgntr        wntr
019600020528     c                   else
019700020531     c     parmag        chain     azorg01l
019800020924     c                   movel     orgde3        og143
019900100930     c                   movel     orgde8        og148
020000020528     c                   endif
020100020920     c                   z-add     §ogas         felluo            3 0
020200921102     C                   Z-ADD     ORGCPF        CAPLUO            5 0
020300921102     C                   MOVEL     ORGDES        RAGLUO           25
020400921102     C                   MOVEL     ORGIND        INDLUO           25
020500921102     C                   MOVEL     ORGLOC        LOCLUO           20
020600921102     C                   MOVEL     ORGPRO        PROLUO            2
020700100924     C                   MOVEL     ORGd70        s_orgd70
020800100924     C                   MOVEL     ORGdit        s_orgdit
020900920507     C*
021000941114     C     KIAVE         SETLL     UNMOV02L                           10
021100941114     C     KIAVE         READE     UNMOV02L                               10
021200100915
021300100915    1C     *IN10         DOWEQ     '0'
021400100915     c
021500100915    2C     UNMTMO        IFNE      'U'
021600100915     c* Escludo i movimenti manuali o automatici a seconda del tipo di stampa da fare
021700100915    3c                   if        (parmaut='S' and unmmaut='S') or
021800100915     c                             (parmaut=' ' and unmmaut=' ')
021900100916     C* PRELEVO DATI UNITA'
022000100916     C     KANA          CHAIN     UNANA01L                           12
022100100916     c
022200101008     c* Solo per movimenti automatici verifico corrispondenza MAG/FOR su ungio
022300100916    4c                   if        parmaut='S'
022400100916     c                   exsr      ctrUNGIO
022500100916    4c                   endif
022600100916     c
022700100916    4c                   if         parmaut=' ' or
022800120524     c                             (parmaut='S' and ungcuf=parcuf
022900120524     c                              and ungfst<>'N' )
023000101013     c
023100100915     c
023200100915     C     UNMDOC        COMP      0                                  202021
023300100915     c* Stampa o ristampa
023400100916    5c                   if        (parris<>'S' and *in21)  or
023500100915     c                             (parris= 'S' and *in20)
023600100915     C
023700120525     c* Verifico se richiesti per ristampa solo le unità in aumento da UNGIO
023800120525   5ac                   if        parris<>'S' or parmaut=' ' or paraum=' ' or
023900101028     c                             (paraum='S' and ungtru='A' )
024000120525     c
024100120525     c* Se devo annullare imposto la "N"  in ungfst per movimenti automatici
024200120525   5bc                   if        parris='A' and parmaut='S'
024300120525
024400120525     c     gio1nrr       chain     ungiofis
024500120525    6c                   if        %found
024600120525     c                   eval      ungfst='N'
024700120525     c                   update    ungiofis
024800120525    6c                   endif
024900120525     c
025000120525  x5bc                   else
025100100915     C                   EXSR      STAMPA
025200120525   5bc                   endif
025300120525   5ac                   endif
025400101028     c
025500101028    5c                   endif
025600100916    4c                   endif
025700100915    3c                   endif
025800100915
025900100915    2C                   ENDIF
026000100915     C     KIAVE         READE     UNMOV02L                               10
026100100915    1C                   ENDDO
026200100916     c
026300100916     c* solo per movimenti automatici ciclo per tipo unità 99
026400101028     c*  e solo se non richieste solo le unità in aumento
026500101028     c                   if        parmaut='S'   and paraum=' '
026600100916     c                   exsr      tipo99
026700100916     c                   endif
026800941114     C*
026900120525     c* Per annullamento vado a fine pgm
027000120525    1c                   if        parris<>'A'
027100120525     c
027200131217     c                   if        parbol='N' or *in15
027300920506     C                   EXCEPT    PIEDE
027400131213     c                   else
027500131213     c                   if        flgai= 1 and not *in15
027600131213     c                   if        felluo<>046 or
027700131213     c                             (felluo=046 and §ogfiue>*zeros)
027800131213     c                   eval      wtvax='P'
027900131213     c                   exsr      sr_fivax
028000131213     c                   endif
028100131213     c                   endif
028200131212     c                   endif
028300920506     C*
028400100915     C* SE flag  = 1 (QUINDI SETRAS) E CAP DIVERSO DA 40138
028500921102     C* SCRIVO IL RECORD DELL'ARCHIVIO SPEDIZIONE
028600131212     C** 15              Z-ADD     NBOL          NUMBOL           15 0
028700131212     C**N15              Z-ADD     BOLLA         NUMBOL           15 0
028800131212     c* stampo segnacolli e lettera di vettura se sono in stampa e ho scritto
028900131212     c* il fivab50f
029000131212     c                   if        not *in15  and *in91 and flgai= 1
029100131212     c                             and parbol<>'N'
029200131212     c                   if        felluo<>046 or
029300131212     c                             (felluo=046 and §ogfiue>*zeros)
029400131212     c                   exsr      PrtScnLdv
029500131212     c                   endif
029600131212     c                   endif
029700120525     c
029800120525    1c                   endif
029900041201     C*
030000041201     c                   clear                   trul33ds
030100041201     C                   MOVEL     'C'           I33TLA
030200041201     c                   movel(p)  trul33ds      kpjbu
030300041201     C                   CALL      'TRUL33R'
030400041201     C                   PARM                    KPJBA
030500131118     C*
030600131118     C                   MOVEL     'C'           D55TLA
030700131118     C                   CALL      'FNLV55R'
030800131118     C                   PARM                    DSLV55
030900131118     C*
031000131118     C                   CLEAR                   DSLV13
031100131118     C                   MOVEL     'C'           I13TLA
031200131118     C                   CALL      'FNLV13R'
031300131118     C                   PARM                    KPJBA
031400131118     C                   PARM                    DSLV13
031500131118     C                   PARM                    DSSI95
031600941114     C*
031700920506     C                   SETON                                        LR
031800941114     C*------------------------------------------------------------------------*
031900941114     C* STAMPA - STAMPA LETTERA DI VETTURA
032000941114     C*------------------------------------------------------------------------*
032100941114     C     STAMPA        BEGSR
032200941114     C*
032300920506     C* SE RISTAMPA LA PRIMA VOLTA LEGGO IL NUMERO DI BOLLA ESISTENTE
032400941114     C   15
032500941114     CANN90              Z-ADD     UNMDOC        NBOL              7 0
032600920506     C   15
032700920506     CANN90              SETON                                        90
032800941114     C*
032900920505     C* PRELEVO DATI UNITA'
033000100916     C**** KANA          CHAIN     UNANA01L                           12
033100941114     C                   MOVEL     UNADES        DESUNI           20
033200101008     c                   movel     unamag        wmag
033300100916     c                   exsr      Partecomune
033400920506     C*
033500131212     c* Se non passo da Wfile non stampo ma devo invece scrivere il fivax
033600131217     c                   if        parbol='N'  or *in15
033700920506     C                   EXCEPT    RIGA
033800920506     C                   ADD       1             CNTR              2 0
033900131212     c                   else
034000131212     c* Creo il fivax
034100131212     c                   if        flgai= 1 and not *in15
034200131212     c                   if        felluo<>046 or
034300131212     c                             (felluo=046 and §ogfiue>*zeros)
034400131212     c                   eval      wtvax='R'
034500131212     c                   exsr      sr_fivax
034600131212     c                   endif
034700131212     c                   endif
034800131213     c                   endif
034900920506     C*
035000941114     C* SE NON E' RISTAMPA AGGIORNO NUMERO DOCUMENTO SUL MOVIMENTO
035100100915    1c                   if        not *in15
035200100915     C                   Z-ADD     BOLLA         UNMDOC
035300100915     C                   EXCEPT    AGG
035400100915     c
035500100915     c* Solo per movimenti automatici
035600100915     c* Aggiorno anche UNGIO col numero della bolla e flag bolla stampata
035700100916    2c                   if        parmaut='S'
035800100915
035900100915     c     gio1nrr       chain     ungiofis
036000100916    3c                   if        %found
036100100915     c                   z-add     bolla         ungdoc
036200100915     c                   eval      ungfst='S'
036300100915     c                   update    ungiofis
036400100916    3c                   endif
036500100915     c
036600100916    2c                   endif
036700100915    1c                   endif
036800920506     C*
036900920505     C                   ENDSR
037000941114     C*------------------------------------------------------------------------*
037100941114     C* CARFBL - SCRIVE UN RECORD NELL'ARCHIVIO SPEDIZIONI
037200941114     C*------------------------------------------------------------------------*
037300921102     C     CARFBL        BEGSR
037400021030     C                   CLEAR                   FIVAB000
037500131118     C                   CLEAR                   FIVAB050
037600150213     c                   clear                   wlog0
037700080924     c* Imposto anno/mese giorno spedizione che mi serve per la pulizia
037800080924     c                   movel     dateu         vabaas
037900080924     c                   move      dateu         vabmgs
038000921102     C*
038100921102     C* CODICE BOLLA PARTENZA
038200140121     c                   if        orgfag='Y'
038300140121     C                   MOVE      '1 '          VABCBO
038400140121     c                   else
038500960104     C                   MOVE      'A '          VABCBO
038600140121     c                   endif
038700131118     C                   MOVEL     '3A'          COD
038800131118     C                   MOVEL     *BLANKS       KEY
038900131203     C                   MOVEL     vabcbo        KEY
039000131118     C     KTAB2         CHAIN     TABEL
039100131118     c                   if        %found(tabel00f)
039200131118     C                   MOVEL     TBLUNI        DS3A
039300131118     c                   else
039400131118     c                   clear                   ds3a
039500131118     c                   movel     'F'           §3atb1
039600131118     c                   endif
039700131118     c
039800921102     C* RAGIONE SOCIALE DESTINATARIO
039900960104     C                   MOVEL     RAGLUO        VABRSD
040000101005     c
040100101005     C* IMPOSTO LA LNA E ZONA 00 SIA PER DESTINATARIO SDI CHE PER BAR
040200101005     C* (PER BAR SOLO SE NON FA PARTE DELL'ARCHIVIO DI APPOGGIO)
040300101005     C     s_orgDIT      IFEQ      'SDI'
040400101005     c     wappoggio     oreq      ' '
040500101005     C                   MOVE      parmag        VABLNA
040600101005     C                   MOVE      00            VABZNC
040700101005     C                   END
040800140409     c* Se archivio di appoggio prendo lna da quest'ultimo se impostata
040900140409     c                   if        wappoggio='S' and wntr>0
041000140409     c                   move      wntr          vablna
041100140409     C                   MOVE      00            VABZNC
041200140409     c                   endif
041300100924     c
041400100924     c* Se si tratta di logistica l'indirizzo lo prendo dalla filiale Bartolini correlata
041500100930     c                   if        §ogfiue>*zeros
041600150213     c                   movel     §ogfiue       wlog0             3 0
041700150213     c     wlog0         chain     azorg01l
041800100924     c                   if        %found(azorg01l)
041900100924     c                   movel     orgind        vabind
042000100924     c                   movel     orgloc        vablod
042100100924     c                   movel     orgpro        vabprd
042200100924     c                   movel     orgcpf        vabcad
042300101005     C     s_orgDIT      IFEQ      'SDI'
042400101005     c     wappoggio     oreq      ' '
042500101005     C                   MOVE      orgfil        VABLNA
042600100924     c                   endif
042700101005     c                   endif
042800100924     c
042900100924     c                   else
043000921102     C* INDIRIZZO DESTINATARIO
043100960104     C                   MOVEL     INDLUO        VABIND
043200921102     C* CAP DESTINATARIO
043300960104     C                   MOVEL     CAPLUO        VABCAD
043400921102     C* LOCALITA' DESTINATARIO
043500960104     C                   MOVEL     LOCLUO        VABLOD
043600921102     C* PROVINCIA DESTINATARIO
043700960104     C                   MOVEL     PROLUO        VABPRD
043800100924     c                   endif
043900100924     c
044000960104     C* NATURA MERCE
044100131118     C                   MOVEL     GAI(2)        VABNAS
044200921102     C* RIFERIMENTO MITTENTE NUMERICO
044300131212     C                   Z-ADD     bolla         VABRMN
044400921102     C* FLAG FERMO DEPOSITO
044500120613     c*  solo se va alla filiale e non a un fornitore (ovvero se si utilizza
044600120613     c*  organigramma di appoggio NO fermio deposito)
044700150213     c* anche per logistica NON imposto il falg fermo deposito
044800150213     c                   if        wappoggio=' '  and (wlog0=0
044900150213     c                             or §ogntw<>'LOG')
045000960104     C                   MOVE      'S'           VABFFD
045100120613     c                   endif
045200960308     C* COD.CLIENTE MITTENTE
045300960308     C                   Z-ADD     1020002       VABCCM
045400980306     C* TIPO SPEDIZIONE
045500980519     C                   MOVE      'C'           VABTSP
045600021030     C* P.O DI GESTIONE
045700021030     C                   MOVEL     VABCCM        VABFGS
045800971024     C* NOTE
045900100924     c                   if        s_orgd70<>*blanks
046000971024     C                   MOVEL     'C.A. SIG'    VABNOT
046100100924     C                   MOVE      s_ORGD70      VABNOT
046200080924     c                   endif
046300100915     c* Peso e colli
046400100915     c                   if        parpkg>0
046500100915     c                   z-add     parpkg        vabpkb
046600100915     c                   endif
046700100915     c                   if        parncl>0
046800100915     c                   z-add     parncl        vabncl
046900100915     c                   endif
047000080924     c
047100131118     C* SCRIVO IL RECORD nel file di appoggio
047200131118    1c                   if        parbol='N'
047300021030     C                   WRITE     FIVAB000
047400131118   x1c                   else
047500131118     C* LINEA  DI PARTENZA
047600131118     C                   Z-ADD     102           VABLNP
047700131118     C                   Z-ADD     102           VABFGS
047800131118     C                   EXSR      CHKCAP
047900131118
048000131118    2c                   if        vablna>0 and o95lna=vablna
048100131118     C                   MOVEL     O95ZNC        VABZNC
048200131118    2C                   END
048300131118    2c                   if        vablna=0
048400131118     C                   MOVEL     O95LNA        VABLNA
048500131118     C                   MOVEL     O95ZNC        VABZNC
048600131118    2c                   endif
048700131118     c
048800131118     C                   MOVEL     O95CTS        VABCTS
048900131118     C                   Z-ADD     01            VABNRS
049000131118     C* NUMERO SPEDIZIONE
049100131118     c* lo prendo da AZNUM con trul33r
049200131118     C                   CLEAR                   TRUL33DS
049300131118     C                   Z-ADD     103           I33CNU
049400131118     C                   Z-ADD     vablnp        I33PO1
049500131118     C                   Z-ADD     1             I33NUM
049600131118     c                   movel(p)  trul33ds      kpjbu
049700131118     c                   call      'TRUL33R'
049800131118     c                   parm                    kpjba
049900131118     c                   movel     kpjbu         trul33ds
050000131118     C                   IF        O33ERR = 0
050100131118     C                   Z-ADD     O33Nrf        vabnsp
050200131118     C                   ELSE
050300131118     C                   CLEAR                   VABNSP
050400131118     C                   ENDIF
050500131118     C
050600131118     C* RICERCO IL NUMERO SEGNACOLLI E LANCIO IL PROGRAMMA STAMPA
050700131118     C* SEGNACOLLI
050800131118     c* lo prendo da AZNUM con trul33r
050900131118     C                   CLEAR                   TRUL33DS
051000131118     C                   Z-ADD     109           I33CNU
051100131118     C                   Z-ADD     vablnp        I33PO1
051200131118     C                   Z-ADD     vabncl        I33NUM
051300131118     c                   movel(p)  trul33ds      kpjbu
051400131118     c                   call      'TRUL33R'
051500131118     c                   parm                    kpjba
051600131118     c                   movel     kpjbu         trul33ds
051700131118     C                   IF        O33ERR<> 0
051800131118     C*
051900131118     C                   Z-ADD     0             VABNCD
052000131118     C                   Z-ADD     0             VABNCA
052100131118     C                   ELSE
052200131118     c                   z-add     o33nri        vabncd
052300131118     c                   z-add     o33nrf        vabnca
052400131118     C                   END
052500131118     C*
052600131118     C* SE AL = DAL AZZERO IL CAMPO
052700131118     C     VABNCA        IFEQ      VABNCD
052800131118     C                   Z-ADD     0             VABNCA
052900131118     C                   END
053000131118     C*
053100131118     C* CODICE TRATTAMENTO MERCE che prevede stampa LDV
053200131213     C                   MOVEL     '3X'          VABCTM
053300131118     C*
053400131118     C*  WRITE
053500131118     C                   WRITE     FIVAB050
053600131118     c
053700131118     C* STAMPO I SEGNACOLLI
053800131118     C* SALVO LA FILIALE CHE STO ELABORANDO
053900131118     C                   MOVEL     ORGFIL        SAVFIL            3 0
054000131212     c                   eval      wtvax='T'
054100131212     c                   exsr      sr_fivax
054200131118     C*
054300131118     C     ENDCRE        TAG
054400131118    1C                   ENDIF
054500921102     C*
054600921102     C                   ENDSR
054700131212     C*------------------------------------------------------------------------*
054800131212     C*  PrtScnLdv - Stampa segnacolli e lettera di vettura se non passa da Wf
054900131212     C*------------------------------------------------------------------------*
055000131212     c* stampa segnacolli e lettera di vettura
055100131212     c     PrtScnLdv     begsr
055200131212     C                   EXSR      GESSNC
055300131212     C*
055400131212     C     SAVFIL        CHAIN     AZORG01L                           95
055500131212     C* STAMPA LETTERA DI VETTURA
055600131212     C                   EXSR      GESLET
055700131212     c                   endsr
055800131118     C*------------------------------------------------------------------------*
055900131118     C*  CHKCAP - CONTROLLO FORMALE DEL CAP
056000131118     C*------------------------------------------------------------------------*
056100131118     C     CHKCAP        BEGSR
056200131118     C*
056300131118     C* IMPOSTO CAMPI DS EST. E LANCIO PGM PER CONTROLLO
056400131118     C                   CLEAR                   DSSI95
056500131118     C                   CLEAR                   DSLV13
056600131118     C* CONTROLLO: 1-FORMALE  2-SU CAPPARIO 3-CONGRUENZA CON PROVINC
056700131118     C                   MOVEL     '3'           I95TCN                                     LLO FORM
056800131118     C                   MOVEL     VABCAD        I95CAP                         *CAP
056900131118     C                   MOVEL     VABPRD        I95PRV
057000131118     C                   MOVEL     VABLOD        I95LOC
057100131118     C                   Z-ADD     vabpkb        I95LKG
057200131118     C                   Z-ADD     vabvlb        I95LMC
057300131118     C                   MOVEL     VABFFD        I95FFD
057400131118     C                   Z-ADD     VABMGS        I95DAT
057500131118     C                   MOVEL     VABAAS        I95DAT
057600131118     C                   MOVEL     'S'           I13AF0
057700131118     C                   MOVEL     'S'           I13CNG
057800131118     C     §3AFCA        IFNE      0
057900131118     C                   MOVEL     'S'           I95FCA
058000131118     C                   ENDIF
058100131118     C                   MOVEL     §3ATB1        I95TPO
058200131118     C                   MOVEL     'S'           I95FRE
058300131118     C                   MOVEL     VABTSP        I95TSP
058400131118     C**
058500131118     C**  TERMINAL DI PARTENZA DELLA LNP
058600131118     C                   CLEAR                   DSLV55
058700131118     C                   MOVEL     'P'           D55TPT
058800131118     C                   MOVE      VABLNP        D55LIN
058900131118     C                   MOVE      VABMGS        D55DRF
059000131118     C                   MOVEL     VABAAS        D55DRF
059100131118     C                   CALL      'FNLV55R'
059200131118     C                   PARM                    DSLV55
059300131118     C**
059400131118     C                   MOVEL     D55TFP        I95TFP
059500131118     C*
059600131118     C                   CALL      'FNLV13R'
059700131118     C                   PARM                    KPJBA
059800131118     C                   PARM                    DSLV13
059900131118     C                   PARM                    DSSI95
060000131118     C**
060100131118     C* ERRORE
060200131203    1C**   O13ERR        IFNE      ' '
060300131118     C* MEMORIZZO SOLTANTO 11 FILIALI SBAGLIATE
060400131203     C**   X             IFLE      11
060500131203     C**                 MOVEL     ORGFIL        FER(X)
060600131203     C**                 ADD       1             X
060700131203     C**                 ENDIF
060800131118     C**
060900131203     C**                 SETON                                        90
061000131203    1C**                 ENDIF
061100131118     C*
061200131118     C                   ENDSR
061300131118     C*---------------------------------------------------------------
061400131118     C*   GESSNC  GESTIONE STAMPA SEGNACOLLI
061500131118     C*---------------------------------------------------------------
061600131118     C*
061700131118     C     GESSNC        BEGSR
061800131118     C*
061900131118     C                   Z-ADD     VABNCD        CO2SCD            7 0
062000131118     C     VABNCA        IFEQ      0
062100131118     C                   Z-ADD     VABNCD        VABNCA
062200131118     C                   END
062300131118     C*
062400131118     C* VALORIZZO LA DS PARAMETRO PASSANTE PER PGM STAMPA SEGNACOLLI
062500131118     C*
062600131118     C                   Z-ADD     0             §LDNC1
062700131118     C                   MOVEL     VABLNP        §LDLNP
062800131118     C     VABLNP        CHAIN     AZORG                              56
062900131118    1C     *IN56         IFEQ      *OFF
063000131118    2C     ORGDE5        IFNE      *BLANKS
063100131118     C                   MOVEL     ORGDE5        §LDDLP
063200131118   X2C                   ELSE
063300131118     C                   MOVEL     ORGDES        §LDDLP
063400131118    2C                   END
063500131118   X1C                   ELSE
063600131118     C                   MOVEL     *BLANKS       §LDDLP
063700131118    1C                   END
063800131118     C                   MOVEL     *BLANK        §LDCDP
063900131118     C                   MOVEL     VABNRS        §LDNRS
064000131118     C                   MOVEL     VABLNA        §LDLNA
064100131118     C     VABLNA        CHAIN     AZORG                              56
064200131118     C   56              MOVEL     *BLANKS       §LDDLA
064300131118    1C  N56ORGDE5        IFNE      *BLANKS
064400131118     C                   MOVEL     ORGDE5        §LDDLA
064500131118     C                   ELSE
064600131118     C                   MOVEL     ORGDES        §LDDLA
064700131118    1C                   END
064800131118     C                   CLEAR                   DSLV55
064900131118     C                   MOVEL     'A'           D55TPT
065000131118     C                   MOVE      VABLNA        D55LIN
065100131118     C                   MOVE      VABMGS        D55DRF
065200131118     C                   MOVEL     VABAAS        D55DRF
065300131118     C                   CALL      'FNLV55R'
065400131118     C                   PARM                    DSLV55
065500131118     C                   MOVEL     D55TFA        §LDTNA
065600131118     C                   MOVEL     VABTSP        §LDTSP
065700131118     C                   MOVEL     VABZNC        §LDZSC
065800131118     C                   MOVE      VABNCL        §LDNC2
065900131118     C* REPERISCO DATI ANAGRAIFCI CLIENTE MITTENTE
066000131118     C                   CLEAR                   DSBS69
066100131118     C                   Z-ADD     VABCCM        I69KAC
066200131118     C                   Z-ADD     VABCCM        I69KIN
066300131118     C                   MOVEL     KNSIF         I69SIF
066400131118     C                   CALL      'TIBS69R'
066500131118     C                   PARM                    DSBS69
066600131118     C                   PARM                    DSACO
066700131118     C                   PARM                    DSIND
066800131118     C                   PARM                    DSCLP
066900131118     C                   PARM                    DSCLS
067000131118     C     O69ERR        IFNE      '1'
067100131118     C     ACOFLG        IFEQ      '*'
067200131118     C                   MOVEL     *BLANKS       §LDRSM
067300131118     C                   ELSE
067400131118     C                   MOVEL     ACORAG        §LDRSM
067500131118     C                   END
067600131118     C                   MOVEL     INDPRV        §LDPRM
067700131118     C                   END
067800131118     C                   MOVEL     VABRSD        §LDRSD
067900131118     C                   MOVEL     VABCAD        §LDCAD
068000131118     C                   MOVEL     VABLOD        §LDLOD
068100131118     C                   MOVEL     VABIND        §LDIND
068200131118     C                   MOVEL     VABPRD        §LDPRD
068300131118     C                   MOVEL     999           §LDCPD
068400131118     C                   MOVEL     *ZEROS        §LDNCP
068500131118     C                   MOVEL     *ZEROS        §LDNCU
068600131118     C     VABVLB        IFGT      0
068700131118     C                   MOVEL     'R'           §LDFVR
068800131118     C                   ELSE
068900131118     C                   CLEAR                   §LDFVR
069000131118     C                   ENDIF
069100131118     C                   Z-ADD     VABVLB        §LDVOL
069200131118     C                   Z-ADD     VABPKB        §LDPKG
069300131118     C                   Z-ADD     VABAAS        §LDAAS
069400131118     C                   Z-ADD     VABMGS        §LDMGS
069500131118     C                   MOVEL     'ITA'         §DETST
069600131118     C     CO2SCD        DOWLE     VABNCA
069700131118     C                   ADD       1             §LDNC1
069800131118     C                   Z-ADD     CO2SCD        §LDNCD
069900131118     C*
070000131203     c                   Call      wd90pss
070100131118     C                   PARM                    DSBLD
070200131118     C                   PARM                    DSBLDE
070300131118     C*
070400131118     C                   ADD       1             CO2SCD
070500131118     C*
070600131118     C                   END
070700131118     C*
070800131118     C                   ENDSR
070900131118     C*---------------------------------------------------------------
071000131118     C*   GESLET  GESTIONE STAMPA LETTERA DI VETTURA
071100131118     C*---------------------------------------------------------------
071200131118     C*
071300131118     C     GESLET        BEGSR
071400131118     C                   CLEAR                   Fnlsb5ds
071500131118     C                   MOVEL     'B'           DB0GES
071600131118     C                   MOVEL     'P'           DB0TBO
071700131118     C*
071800131118     C                   Z-ADD     VABLNP        DB0LNP
071900131118     C                   Z-ADD     VABAAS        DB0AAS
072000131118     C                   Z-ADD     VABMGS        DB0MGS
072100131118     C                   Z-ADD     VABNRS        DB0NRS
072200131118     C                   Z-ADD     VABNSP        DB0NSP
072300131118     C                   MOVE      VABCBO        DB0CBO
072400131118     C                   MOVEL     §3ATB1        DB0TB1
072500131118     C                   MOVEL     §3ATB2        DB0TB2
072600131118     C                   MOVEL     §3ADES        DB0DCB
072700131118     C                   Z-ADD     VABLNA        DB0LNA
072800131118     C* PASSO INDIRIZZO COMPLETO DELLA LNA
072900131118     C     VABLNA        CHAIN     AZORG                              30
073000131118     C     *IN30         IFEQ      *OFF
073100131118     C                   MOVEL     ORGFAG        DB0FAG
073200131118     C                   MOVEL     ORGDES        DB0DLA
073300131118     C                   MOVEL     ORGIND        DB0INL
073400131118     C                   MOVEL     ORGLOC        DB0LOL
073500131118     C                   MOVEL     ORGPRO        DB0PRL
073600131118     C                   MOVEL     ORGTEL        DB0TEL
073700160519     C                   MOVEL     ORGdd2        DB0FAX
073800131118     c                   Movel     OrgCpf        Db0Cal
073900131118     C                   ENDIF
074000131118     C                   Z-ADD     VABZNC        DB0ZNC
074100131118     C* MITTENTE
074200131118     C                   MOVEL     VABCCM        DB0CCM
074300131118     C                   MOVEL     ACORAG        DB0RSM
074400131118     C                   MOVEL     INDVIA        DB0INM
074500131118     C                   MOVEL     INDCAE        DB0CAM
074600131118     C                   MOVEL     INDCIT        DB0LOM
074700131118     C                   MOVEL     INDPRV        DB0PRM
074800131118     C                   MOVEL     INDSTA        DB0NZM
074900131118     C                   MOVEL     VABRSD        DB0RSD
075000131118     C                   MOVEL     VABIND        DB0IND
075100131118     C                   MOVEL     VABLOD        DB0LOD
075200131118     C                   MOVEL     VABPRD        DB0PRD
075300131118     C                   MOVEL     VABCAD        DB0CAD
075400131118     C                   MOVEL     VABNZD        DB0NZD
075500131118     C* FLAG INOLTRO  --> SE FERMO DEPOSITO SEMPRE 'C'
075600131118     c*  ai fini della stampa il campo non serve per cui preferisco non pasarlo in vista
075700131118     c*  dei nuovi tipi inoltro per centro storico
075800131118     C                   MOVEL     VABGC1        DB0GC1
075900131118     C                   MOVEL     VABGC2        DB0GC2
076000131118     C                   MOVEL     VABTSP        DB0TSP
076100131118     C* DECODIFICA TIPO SERVIZIO
076200131118     C     VABTSP        IFNE      ' '
076300131118     C                   MOVEL     '5E'          COD
076400131118     C                   MOVEL(P)  VABTSP        KEY
076500131118     C     KTAB2         CHAIN     TABEL                              30
076600131118     C  N30              MOVEL     TBLUNI        DS5E
076700131118     C   30              CLEAR                   DS5E
076800131118     C                   MOVEL     §5EDES        DB0DTS
076900131118     C                   MOVEL     §5EFST        DB0FST
077000131118     C                   ENDIF
077100131118     C*
077200131118     C                   Z-ADD     VABIAS        DB0IAS
077300131118     C                   MOVEL     VABVAS        DB0VAS
077400131118     C                   MOVEL     VABNAS        DB0NAS
077500131118     C                   Z-ADD     VABNCL        DB0NCL
077600131118     C                   Z-ADD     VABPKB        DB0PKF
077700131118     C                   Z-ADD     VABVLB        DB0VLF
077800131118     C     DB0VLF        IFGT      0
077900131118     C                   MOVEL     'R'           DB0FVF
078000131118     C                   ENDIF
078100131118     C                   Z-ADD     VABRMN        DB0RMN
078200131118     C                   Z-ADD     VABLNP        DB0FLS
078300131118     C                   Z-ADD     VABNCD        DB0NCD
078400131118     C                   Z-ADD     VABNCA        DB0NCA
078500131118     C     DB0NCA        IFEQ      DB0NCD
078600131118     C                   CLEAR                   DB0NCA
078700131118     C                   ENDIF
078800131118     C                   MOVEL     VABXCO        DB0XCO
078900131118     C                   MOVEL     VABNOT        DB0NT1
079000131118     C                   MOVEL     VABNT2        DB0NT2
079100131118     C                   MOVEL     VABFFD        DB0FFD
079200131118     C*                                             c
079300140116     c******             call      wD90psl
079400140116     C******             PARM                    Fnlsb5ds
079500131118     C*
079600131118     C                   ENDSR
079700100916     c*------------------------------------------------------------------------*
079800100916     c* Controllo su UNGIO corrrispondenza MAG/FOR
079900100916     c*------------------------------------------------------------------------*
080000100916     C     CtrUNGIO      BEGSR
080100100916     c
080200101028     c                   clear                   ungtru
080300101028     c                   clear                   ungmag
080400101013     c                   clear                   ungcuf
080500101013     c                   clear                   gio1NRR
080600100916     c     kungio        setll     ungio01l
080700100916     c     kungio        reade     ungio01l
080800101013    2c                   dow       not %eof(ungio01l)
080900100916     c
081000101011    3c                   if        ungdtm=pardat and unmmag=ungmag
081100100916     c                   leave
081200100916     c                   else
081300100916     c                   clear                   ungcuf
081400101028     c                   clear                   ungtru
081500100916    3c                   endif
081600100916     c
081700100916     c     kungio        reade     ungio01l
081800100916    2c                   enddo
081900100916     c
082000100916     C                   ENDSR
082100100916     c*------------------------------------------------------------------------*
082200100916     c* Per movimenti automatici stampo in fondo il tipo "99"
082300100916     c*------------------------------------------------------------------------*
082400100916     C     Tipo99        BEGSR
082500100916     c     kungio3       setll     ungio03l
082600100916     c     kungio3       reade     ungio03l
082700100916    1c                   dow       not %eof(ungio03l)
082800100916     c
082900101013    2c                   if        ungtip=99  and ungcuf=parcuf
083000120525     c                             and ungfst<>'N'
083100101013     c
083200100916    3c                   if        (parris<>'S' and ungdoc=0)  or
083300100916     c                             (parris= 'S' and ungdoc>0)
083400120525     c
083500120525     c* se devo annullare imposto "N" in ungfst
083600120525   3ac                   if        parris='A'
083700120525    4c                   if        not *in15
083800120525     c                   eval      ungfst='N'
083900120525     c                   update    ungio3
084000120525    4c                   endif
084100120525     c
084200120525     c                   else
084300100916     c
084400100916     c* In ristampa prendo il numero bolla se non ancora fatto
084500100916    4c                   if        *in15 and not *in90
084600100916     C                   Z-ADD     UNgDOC        NBOL
084700100916     c                   seton                                        90
084800100916    4c                   endif
084900100916     c
085000100916     c* Stampa: parte comune a UNMOV
085100101008     c                   movel     ungmag        wmag
085200100916     c                   exsr      Partecomune
085300100916
085400100916     c* stampo riga di dettaglio UNGIO
085500131217     c                   if        parbol='N' or *in15
085600100916     c                   except    rigaUNG
085700140121     c                   else
085800140121     c* Creo il fivax
085900140121     c                   if        flgai= 1 and not *in15
086000140121     c                   if        felluo<>046 or
086100140121     c                             (felluo=046 and §ogfiue>*zeros)
086200140121     c                   eval      wtvax='9'
086300140121     c                   exsr      sr_fivax
086400140121     c                   endif
086500140121     c                   endif
086600131212     c                   endif
086700100916     c
086800100916     c* Aggiorno record UNGIO
086900100916    4c                   if        not *in15
087000100916     c                   z-add     bolla         ungdoc
087100100916     c                   eval      ungfst='S'
087200100916     c                   update    ungio3
087300100916    4c                   endif
087400120525   3ac                   endif
087500100916     c
087600100916    3c                   endif
087700100916    2c                   endif
087800100916     c
087900100916     c     kungio3       reade     ungio03l
088000100916    1c                   enddo
088100100916     c                   ENDSR
088200100916     c*------------------------------------------------------------------------*
088300100916     c* Parte comune per la stampa a unmov e ungio
088400100916     c*------------------------------------------------------------------------*
088500100916     c     Partecomune   BEGSR
088600100916     C**
088700100916     C* OPERAZIONE DA ESEGUIRE SOLO UNA VOLTA
088800100916    1C                   IF         not *in91
088900100916     c
089000100916     C* SOLO IN STAMPA PRELEVO IL NUMERO DEL C/S
089100100916    2C                   if        not *in15
089200100916     c* lo prendo da AZNUM con trul33r
089300100916     C                   CLEAR                   TRUL33DS
089400100916     C                   Z-ADD     50            I33CNU
089500100916     C                   Z-ADD     1             I33NUM
089600100916     c                   movel(p)  trul33ds      kpjbu
089700100916     c                   call      'TRUL33R'
089800100916     c                   parm                    kpjba
089900100916     c                   movel     kpjbu         trul33ds
090000100916    3C                   IF        O33ERR = 0
090100100916     C                   Z-ADD     O33Nrf        bolla             7 0
090200100916     c                   else
090300100916     c                   clear                   bolla
090400100916    3c                   endif
090500131212     c
090600131212     c* Creo il record bolla
090700131212     c                   if        flgai= 1
090800131212     c                   if        felluo<>046 or
090900131212     c                             (felluo=046 and §ogfiue>*zeros)
091000131212     C                   EXSR      CARFBL
091100131212     C                   END
091200131212     c                   endif
091300100916     c
091400100916    2C                   endif
091500100916     C**
091600131217     c                   if        parbol='N' or *in15
091700100916     C                   EXCEPT    TESTA
091800131212     c                   endif
091900100916     C                   SETON                                        91
092000100916    1C                   ENDIF
092100100916     C*
092200131217     c                   if        parbol='N'  or *in15
092300100916     C* CONTO LE RIGHE STAMPATE A SALTO PAGINA RIAZZERO
092400100916    1C     CNTR          IFGE      20
092500100916     C                   Z-ADD     0             CNTR
092600100916     C     PAGE          ADD       1             PAG
092700100916     C                   EXCEPT    PRI
092800100916     C                   EXCEPT    TESTA
092900100916     C     PAGE          SUB       1             PAG               3 0
093000100916     C                   EXCEPT    PRO
093100100916     C                   ADD       1             CNTR
093200100916    1C                   END
093300131212     c                   endif
093400100916     c
093500100916     c                   ENDSR
093600131212     c*------------------------------------------------------------------------*
093700131212     c* Scrittura del fivax
093800131212     c*------------------------------------------------------------------------*
093900131212     c     sr_fivax      BEGSR
094000131212     c                   eval      VAXATB=vabatb
094100131212     c                   eval      VAXFGS=vabfgs
094200131212     c                   eval      VAXCCM=vabccm
094300131212     c                   eval      VAXLNP=vablnp
094400131212     c                   eval      VAXAAS=vabaas
094500131212     c                   eval      VAXNRS=vabnrs
094600131212     c                   eval      VAXNSP=vabnsp
094700131212     c                   eval      VAXTMD='STD1'
094800131212     c                   select
094900131212     c* scrittura tipi record di testata: C1 e T1
095000131212     c                   when      wtvax='T'
095100131212     c                   eval      VAXTRK='C1'
095200131213     c                   eval      prg=prg+1
095300131213     c                   eval      VAXPRR=prg
095400131213     c                   eval      VAXDTT='Conto Servizio n. ' +
095500131213     c                             %editc(bolla:'Z') + '         ' +
095600131213     c                             %editW(udate:'  /  /  ')
095700131213     c                   write     fivax000
095800131212     c                   eval      VAXTRK='T1'
095900131213     c                   eval      prg=prg+1
096000131213     c                   eval      VAXPRR=prg
096100131213     c                   eval      VAXDTT=('alla cortese attenzione Sig. '
096200140218     c                             + s_orgd70)
096300131213     c                   write     fivax000
096400131213     c                   eval      VAXTRK='C1'
096500131213     c                   eval      prg=prg+1
096600131213     c                   eval      VAXPRR=prg
096700131213     c                   eval      VAXDTT=('  Materiale Descrizione' +
096800131213     c                             '          ' + 'Marca           ' +
096900131213     c                             'Modello         ' + 'Matricola')
097000131213     c                   write     fivax000
097100131213     c                   eval      VAXTRK='C1'
097200131213     c                   eval      prg=prg+1
097300131213     c                   eval      VAXPRR=prg
097400131213     c                   eval      VAXDTT=('------------------------------
097500131213     c                             ----------------------------------------
097600131213     c                             ------------------------------')
097700131213     c                   write     fivax000
097800131212     c* scrittura record di riga
097900131212     c                   when      wtvax='R'
098000131212     c                   eval      VAXTRK='C1'
098100131213     c                   eval      prg=prg+1
098200131213     c                   eval      VAXPRR=prg
098300131213     c                   eval      VAXDTT='  ' + %editc(unmtip:'X') + '  ' +
098400131213     c                             %editc(unmcod:'X') + ' ' +
098500131213     c                             desuni + ' ' + unamar + ' ' + unamod +
098600131213     c                             ' ' + unamat
098700131213     c                   write     fivax000
098800140313     c* se presente codice cliente aggiungo altra riga per specificare se comodato in
098900140313     c* aumento o sostituzione + cod.cliente e ragione sociale
099000140313     c                   if        ungksc>*zeros
099100140313     c                   exsr      sr_AumSost
099200140313     c                   endif
099300131213     c* scrittura record di riga da giro 99
099400131213     c                   when      wtvax='9'
099500131213     c                   eval      VAXTRK='C1'
099600131213     c                   eval      prg=prg+1
099700131213     c                   eval      VAXPRR=prg
099800131213     c                   eval      VAXDTT='  ' + %editc(ungtip:'X') + ' Qta ' +
099900131213     c                             %editc(ungqta:'Z') + ' ' +
100000131213     c                             ungdes + '       ' + ungnot
100100131213     c                   write     fivax000
100200140313     c                   if        ungksc>*zeros
100300140313     c                   exsr      sr_AumSost
100400140313     c                   endif
100500131213     c* scrittura record di piede
100600131213     c                   when      wtvax='P'
100700131213     c                   eval      VAXTRK='C1'
100800131213     c                   eval      prg=prg+1
100900131213     c                   eval      VAXPRR=prg
101000131213     c                   eval      VAXDTT=('------------------------------
101100131213     c                             ----------------------------------------
101200131213     c                             ------------------------------')
101300131213     c                   write     fivax000
101400131213     c                   eval      VAXTRK='C1'
101500131213     c                   eval      prg=prg+1
101600131213     c                   eval      VAXPRR=prg
101700131213     c                   eval      VAXDTT=('   Colli:    ' + %editc(parncl:'2')
101800131213     c                             )
101900131213     c                   write     fivax000
102000131213     c                   eval      VAXTRK='C1'
102100131213     c                   eval      prg=prg+1
102200131213     c                   eval      VAXPRR=prg
102300131213     c                   eval      VAXDTT=('  Peso Kg:' + '  ' +
102400131213     c                             %editc(parpkg:'2'))
102500131213     c                   write     fivax000
102600131212     c                   endsl
102700131212     c                   endsr
102800140313     c*------------------------------------------------------------------------*
102900140313     c*
103000140313     c*------------------------------------------------------------------------*
103100140313     c     sr_AumSost    BEGSR
103200140313     C* REPERISCO DATI ANAGRAIFCI CLIENTE MITTENTE
103300140313     C                   CLEAR                   DSBS69
103400140313     c                   clear                   acorag
103500140313     C                   Z-ADD     ungksc        I69KAC
103600140313     C                   MOVEL     KNSIF         I69SIF
103700140313     C                   CALL      'TIBS69R'
103800140313     C                   PARM                    DSBS69
103900140313     C                   PARM                    DSACO
104000140313     C                   PARM                    DSIND
104100140313     C                   PARM                    DSCLP
104200140313     C                   PARM                    DSCLS
104300140313     c                   eval      VAXTRK='C1'
104400140313     c                   eval      prg=prg+1
104500140313     c                   eval      VAXPRR=prg
104600140313     c                   eval      VAXDTT='  Comodato in'
104700140313     c                   if        ungtru=*blanks or ungtru='A'
104800140313     c                   eval      VAXDTT=%trimr(vaxdtt) + ' aumento'
104900140313     c                   else
105000140313     c                   eval      VAXDTT=%trimr(vaxdtt) + ' sostituzione'
105100140313     c                   endif
105200140313     c                   eval      vaxdtt=%trimr(vaxdtt) + ' al cliente: '
105300140313     c                             + %editc(ungksc:'X') + ' ' + acorag
105400140313     c                   write     fivax000
105500140313     c                   endsr
105600100916     O*------------------------------------------------------------------------*
105700140207     OFNLV22P   T    LRNLR
105800100916     O* AGG - AGGIORNAMENTO NUMERO DOCUMENTO MOVIMENTI UNITA'
105900100916     O*------------------------------------------------------------------------*
106000100916     OUNMOV000  E            AGG
106100941114     O                       UNMDOC
106200941114     O*------------------------------------------------------------------------*
106300101008     OLET132    E            TESTA            06
106400120322     O                       RAGIO218            32
106500920505     O                                           54 'Conto Servizio n.'
106600920506     O              N15      BOLLA         Z     62
106700920506     O               15      NBOL          Z     62
106800941115     O                       UDATE               80 '  /  /  '
106900920504     O          E            TESTA            07
107000120322     O                       ind_218             32
107100941115     O                                           74 'Pag.'
107200941115     O                       PAGE                80
107300920504     O          E            TESTA            08
107400120322     O                       cap_218              7
107500120322     O                       loc_218           +  1
107600120322     O                       prv_218           +  1
107700920504     O          E            TESTA            09
107800941115     O                       TRA                 80
107900920504     O          E            TESTA            10
108000920504     O                                           25 'Destinatario e luogo di'
108100920504     O                                         +  1 'destinazione'
108200920504     O          E            TESTA            11
108300941115     O                       TRA                 80
108400920504     O          E            TESTA            12
108500101008     O                       wmag                10
108600960927     O                                         +  2 'P.Operativo'
108700941114     O                       ORGDES            +  1
108800920505     O          E            TESTA            13
108900971024     O                       ORGIND              44
109000941115     O          E            TESTA            14
109100971024     O                       ORGCPF              29
109200941114     O                       ORGLOC            +  1
109300941114     O                       ORGPRO            +  1
109400941114     O          E            TESTA            16
109500920505     O                                         +  2 'alla cortese attenzione'
109600920505     O                                         +  1 'Sig.'
109700920505     O                       ORGD70            +  1
109800941114     O          E            TESTA       0  0
109900920505     O                                         +  2 'alla cortese attenzione'
110000920505     O                                         +  1 'Sig.'
110100920505     O                       ORGD70            +  1
110200920505     O          E            TESTA            18
110300941115     O                       TRA                 80
110400920505     O          E            TESTA            19
110500920505     O                                           09 'Vettore'
110600920505     O          E            TESTA            20
110700920505     O                       RAGIO               32
110800920505     O                       SIMVIA            +  1
110900920505     O                       SIMPRV            +  1
111000920505     O          E            TESTA            21
111100941115     O                       TRA                 80
111200920505     O          E            TESTA            22
111300920505     O                                           11 'Materiale'
111400941115     O                                           23 'Descrizione'
111500941115     O                                           38 'Marca'
111600941115     O                                           56 'Modello'
111700941115     O                                           74 'Matricola'
111800920505     O          E            TESTA            23
111900941115     O                       TRA                 80
112000920505     O*
112100920505     O          E            RIGA        1
112200941115     O                       UNMTIP               4
112300941115     O                       UNMCOD              11
112400941115     O                       DESUNI              32
112500941115     O                       UNAMAR              48
112600941115     O                       UNAMOD              64
112700110407     O                       UNmMAT              80
112800100916     o
112900100916     O          E            RIGAung     1
113000100916     O                       UNgTIP               4
113100100916     o                                            8 'Qta'
113200100916     O                       UNgqta        z     11
113300110914     O                       ungdes              42
113400100916     O                       UNgnot              69
113500920506     O          E            PIEDE       2
113600941115     O                       TRA                 80
113700920506     O          E            PIEDE       1
113800920505     O                                           09 'Colli:'
113900100915     O                       parncl        2  +   4
114000971024     O               15N15   TES(1)              75
114100920506     O          E            PIEDE       1
114200971024     O               15N15   TES(2)              75
114300920508     O          E            PIEDE       1
114400920508     O                                           10 'Peso Kg:'
114500100915     O                       parpkg        2  +   2
114600920506     O          E            PIEDE       3
114700971024     O                                           60 'IMS - SEDE'
114800920506     O          E            PIEDE       2  2
114900920505     O                                           62 '______________'
115000971024     O**      E 3              PIEDE
115100971024     O**                                 25 'Ritirato il _________'
115200971024     O**      E 2              PIEDE
115300971024     O**                                 25 '_____________________'
115400920506     O          E            PRO         1
115500920506     O                                           18 'Prosegue da pag.'
115600920506     O                       PAG               +  1
115700920506     O          E            PRI         1
115800920506     O                                           62 'Segue a pag.'
115900920506     O                       PAG               +  1
116000971024**   Testo
116100920504Merci Esenti Emissione Bolla Accompagnamento
116200920504Merci ai Sensi D.P.R. 6/10/78 N. 627 Art. 4
116300921102** GAI TABELLA CONTROLLO SISTEMA INFORMATIVO
116400960105GAITRA201
116500140207MATERIALE ICT
116600100920GAITRAPSP
