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
002700100924     d s_orgdit        s                   like(orgdit)
002800101008     d wmag            s                   like(unamag)
002900131212     d wtvax           s              1    inz
003000131213     d prg             s              4  0 inz
003100020528
003200041201     d trul33ds      e ds
003300131118     d
003400941114     D KPJBA         E DS
003500941114     D PARAM           DS
003600941114     D  PARMAG                 1      3  0
003700941114     D  PARDAT                 4     11  0
003800941114     D  PARRIS                12     12
003900100910     D  PARncl                13     17  0
004000100910     D  PARpkg                18     24  1
004100100910     D  PARMAUT               25     25
004200100915     D  PARcuf                26     28
004300101028     D  PARAUM                29     29
004400131203     D  PARbol                30     30
004500131203     d* - parametri da TRUL90R:
004600131203     d* · pgm.   per stampa LDV
004700131203     d  wD90psl              157    166
004800131203     d* · stampante per LDV in formato "A4" (laser)
004900131203     d  wD90prb4             167    176
005000131203     d* · stampante per LDV in formato "A5" (laser)
005100131203     d  wD90prb5             177    186
005200131203     d* · modulo per stampa LDV su "A4"     (laser)
005300131203     d  wD90mdb4             187    196
005400131203     d* · modulo per stampa LDV su "A5"     (laser)
005500131203     d  wD90mdb5             197    206
005600131203     d* · pgm di stampa etichette segnacolli
005700131203     d  wD90pss              207    216
005800131203     d* · modulo per stampa segnacolli
005900131203     d  wD90mde              227    236
006000131203     d* · stampante per segnacolli
006100131203     d  wD90pre              247    256
006200131203     d
006300100910     d
006400941114     D CNCR40        E DS
006500120322     D CNCR42        E DS
006600941114     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
006700910610     D                 DS
006800020920
006900131203     D DS5E          E DS
007000131118     d ds3a          e ds
007100131118     d og143         e ds
007200100930     d og148         e ds
007300131118     D DSLV55        E DS                  EXTNAME(FNLV55DS)
007400131118     D DSSI95        E DS                  EXTNAME(TISI95DS)
007500131118     D DSLV13        E DS                  EXTNAME(FNLV13DS)
007600131203     D DSBLD         E DS
007700131203     D* STAMPANTE SEGNACOLLI
007800131203     D* DS ESTERA PER  STAMPA BOLLE
007900131203     d Fnlsb5ds      e ds
008000131203     d
008100131203     D DSBLDE        E DS                  INZ
008200080924     d
008300080924     D WLBDAT          DS                  INZ
008400080924     D  G02DAT                 1      8  0
008500080924     D  G02INV                 9     16  0
008600080924     D  G02ERR                17     17
008700080924     D  G02TGI                18     22  0
008800131203     D DSACO         E DS                  EXTNAME(CNACO00F)
008900131203     D DSIND         E DS                  EXTNAME(CNIND00F)
009000131203     D DSCLP         E DS                  EXTNAME(CNCLP00F)
009100131203     D DSCLS         E DS                  EXTNAME(FNCLS00F)
009200131203     D DSBS69        E DS                  EXTNAME(TIBS69DS)
009300120322     d
009400120322     d  ragio218       s             30
009500120322     d  loc_218        s             30
009600120322     d  ind_218        s             30
009700120322     d  prv_218        s              2
009800120322     d  cap_218        s              5  0
009900120322     d  wfil           s              3  0
010000140409     d  wntr           s                   like(vablna)
010100020322
010200941115     C*------------------------------------------------------------------------*
010300941115     C* INDICATORI
010400941115     C*------------------------------------------------------------------------*
010500941115     C* 10 - CAUSALE NON PRESENTE
010600941115     C* 11 - MAGAZZINO INESISTENTE
010700941115     C* 12 - QUANTITA' OBBLIGATORIA
010800941115     C* 13 - MAGAZZINO CORRELATO INESISTENTE
010900941115     C* 14 - DATA NON VALIDA
011000941115     C* 16 - ON SE C'E' CAUSALE CORRELATA
011100941114     C*------------------------------------------------------------------------*
011200941114     C* MAIN LINE
011300941114     C*------------------------------------------------------------------------*
011400941114     C* RICEVIMENTO PARAMETRI
011500941114     C     *ENTRY        PLIST
011600941114     C                   PARM                    KPJBA
011700941114     C                   Z-ADD     1             CODUT
011800941114     C                   CALL      'X§PARUT'
011900941114     C                   PARM                    UTEDSE
012000941114     C                   MOVEL     REC40         CNCR40
012100120322     C                   MOVEL     REC42         CNCR42
012200120322     C                   MOVEL     simrgs        RAGIO            30
012300120322     C*****              MOVEL     'SETRAS  '    RAGIO            30
012400941115     C                   MOVEL     *ALL'-'       TRA              80
012500120322     C
012600120322     C* USO FILIALE 218 PER L'INDIRIZZO DI SETRAS
012700120322     C                   EVAL      WFIL=218
012800120322     C     wfil          CHAIN     AZORG21l
012900120322     c                   if        %found(azorg21l)
013000120322     c                   eval      ragio218=orgdes
013100120322     c                   eval      ind_218=orgind
013200120322     c                   eval      loc_218=orgloc
013300120322     c                   eval      prv_218=orgpro
013400120322     c                   eval      cap_218=orgcpf
013500120322     c                   endif
013600941114     C*
013700960105     C* CONTROLLO SE IL SISTEMA INFORMATIVO  E' RIFERITO A GAITRA201
013800941114     C     KNSIF         IFEQ      GAI(1)
013900100920     C     KNSIF         oreq      GAI(3)
014000021030     C                   OPEN      FIVABWKF
014100921102     C                   Z-ADD     1             FLGAI             1 0
014200941114     C                   END
014300080924     c* Prendo la data del giorno
014400080924     C**
014500080924     C                   TIME                    W0140            14 0
014600080924     C                   MOVE      W0140         WDTGIO            8 0
014700080924     C*
014800080924     C                   Z-ADD     WDTGIO        G02DAT
014900080924     C                   MOVEL     *BLANK        G02ERR
015000080924     C                   CALL      'XSRDA8'
015100080924     C                   PARM                    WLBDAT
015200080924     C                   MOVEL     G02INV        DATEU             8 0
015300900116     C*
015400941114     C* LETTURA FLNUF01L - COMPLETA
015500041201     C**   KNUF          KLIST
015600041201     C**                 KFLD                    YEAR              2 0
015700041201     C**                 KFLD                    CODN              3 0
015800941114     C*
015900941114     C* LETTURA UNMOV02L - COMPLETA
016000920505     C     KIAVE         KLIST
016100941114     C                   KFLD                    PARMAG
016200971013     C                   KFLD                    PARDAT
016300941114     C*
016400941114     C* LETTURA UNANA01L - COMPLETA
016500941114     C     KANA          KLIST
016600941114     C                   KFLD                    UNMTIP
016700941114     C                   KFLD                    UNMCOD
016800100915     C* LETTURA UNgio01l -
016900100915     C     Kungio        KLIST
017000100915     C                   KFLD                    UNMTIP
017100110407     C                   KFLD                    unmmat
017200100915     C                   KFLD                    parcuf
017300100916     C* LETTURA UNgio03l -
017400100916     C     Kungio3       KLIST
017500100916     C                   KFLD                    parcuf
017600100916     C                   KFLD                    pardat
017700100916     C                   KFLD                    parmag
017800131118     C     KTAB2         KLIST
017900131118     C                   KFLD                    CODUT
018000131118     C                   KFLD                    COD               2
018100131118     C                   KFLD                    KEY               8
018200941114     C*
018300920505     C                   MOVEL     KPJBU         PARAM
018400960105     C* 15 ON - RISTAMPA
018500941114     C     PARRIS        COMP      'S'                                    15
018600920505     C* PRELEVO DATI FILIALE
018700020528     C                   clear                   wappoggio
018800020924     c                   Clear                   og143
018900100930     c                   Clear                   og148
019000020531     C     PARMAG        CHAIN     AZORG21l
019100110613     c                   if        %found(azorg21l) and orgfag<>' '
019200020528     c                   eval      wappoggio = 'S'
019300140409     c* memorizzo eventuale lna presente in orgntr
019400140409     c                   move      orgntr        wntr
019500020528     c                   else
019600020531     c     parmag        chain     azorg01l
019700020924     c                   movel     orgde3        og143
019800100930     c                   movel     orgde8        og148
019900020528     c                   endif
020000020920     c                   z-add     §ogas         felluo            3 0
020100921102     C                   Z-ADD     ORGCPF        CAPLUO            5 0
020200921102     C                   MOVEL     ORGDES        RAGLUO           25
020300921102     C                   MOVEL     ORGIND        INDLUO           25
020400921102     C                   MOVEL     ORGLOC        LOCLUO           20
020500921102     C                   MOVEL     ORGPRO        PROLUO            2
020600100924     C                   MOVEL     ORGdit        s_orgdit
020700920507     C*
020800941114     C     KIAVE         SETLL     UNMOV02L                           10
020900941114     C     KIAVE         READE     UNMOV02L                               10
021000100915
021100100915    1C     *IN10         DOWEQ     '0'
021200100915     c
021300100915    2C     UNMTMO        IFNE      'U'
021400100915     c* Escludo i movimenti manuali o automatici a seconda del tipo di stampa da fare
021500100915    3c                   if        (parmaut='S' and unmmaut='S') or
021600100915     c                             (parmaut=' ' and unmmaut=' ')
021700100916     C* PRELEVO DATI UNITA'
021800100916     C     KANA          CHAIN     UNANA01L                           12
021900100916     c
022000101008     c* Solo per movimenti automatici verifico corrispondenza MAG/FOR su ungio
022100100916    4c                   if        parmaut='S'
022200100916     c                   exsr      ctrUNGIO
022300100916    4c                   endif
022400100916     c
022500100916    4c                   if         parmaut=' ' or
022600120524     c                             (parmaut='S' and ungcuf=parcuf
022700120524     c                              and ungfst<>'N' )
022800101013     c
022900100915     c
023000100915     C     UNMDOC        COMP      0                                  202021
023100100915     c* Stampa o ristampa
023200100916    5c                   if        (parris<>'S' and *in21)  or
023300100915     c                             (parris= 'S' and *in20)
023400100915     C
023500120525     c* Verifico se richiesti per ristampa solo le unità in aumento da UNGIO
023600120525   5ac                   if        parris<>'S' or parmaut=' ' or paraum=' ' or
023700101028     c                             (paraum='S' and ungtru='A' )
023800120525     c
023900120525     c* Se devo annullare imposto la "N"  in ungfst per movimenti automatici
024000120525   5bc                   if        parris='A' and parmaut='S'
024100120525
024200120525     c     gio1nrr       chain     ungiofis
024300120525    6c                   if        %found
024400120525     c                   eval      ungfst='N'
024500120525     c                   update    ungiofis
024600120525    6c                   endif
024700120525     c
024800120525  x5bc                   else
024900100915     C                   EXSR      STAMPA
025000120525   5bc                   endif
025100120525   5ac                   endif
025200101028     c
025300101028    5c                   endif
025400100916    4c                   endif
025500100915    3c                   endif
025600100915
025700100915    2C                   ENDIF
025800100915     C     KIAVE         READE     UNMOV02L                               10
025900100915    1C                   ENDDO
026000100916     c
026100100916     c* solo per movimenti automatici ciclo per tipo unità 99
026200101028     c*  e solo se non richieste solo le unità in aumento
026300101028     c                   if        parmaut='S'   and paraum=' '
026400100916     c                   exsr      tipo99
026500100916     c                   endif
026600941114     C*
026700120525     c* Per annullamento vado a fine pgm
026800120525    1c                   if        parris<>'A'
026900120525     c
027000131217     c                   if        parbol='N' or *in15
027100920506     C                   EXCEPT    PIEDE
027200131213     c                   else
027300131213     c                   if        flgai= 1 and not *in15
027400131213     c                   if        felluo<>046 or
027500131213     c                             (felluo=046 and §ogfiue>*zeros)
027600131213     c                   eval      wtvax='P'
027700131213     c                   exsr      sr_fivax
027800131213     c                   endif
027900131213     c                   endif
028000131212     c                   endif
028100920506     C*
028200100915     C* SE flag  = 1 (QUINDI SETRAS) E CAP DIVERSO DA 40138
028300921102     C* SCRIVO IL RECORD DELL'ARCHIVIO SPEDIZIONE
028400131212     C** 15              Z-ADD     NBOL          NUMBOL           15 0
028500131212     C**N15              Z-ADD     BOLLA         NUMBOL           15 0
028600131212     c* stampo segnacolli e lettera di vettura se sono in stampa e ho scritto
028700131212     c* il fivab50f
028800131212     c                   if        not *in15  and *in91 and flgai= 1
028900131212     c                             and parbol<>'N'
029000131212     c                   if        felluo<>046 or
029100131212     c                             (felluo=046 and §ogfiue>*zeros)
029200131212     c                   exsr      PrtScnLdv
029300131212     c                   endif
029400131212     c                   endif
029500120525     c
029600120525    1c                   endif
029700041201     C*
029800041201     c                   clear                   trul33ds
029900041201     C                   MOVEL     'C'           I33TLA
030000041201     c                   movel(p)  trul33ds      kpjbu
030100041201     C                   CALL      'TRUL33R'
030200041201     C                   PARM                    KPJBA
030300131118     C*
030400131118     C                   MOVEL     'C'           D55TLA
030500131118     C                   CALL      'FNLV55R'
030600131118     C                   PARM                    DSLV55
030700131118     C*
030800131118     C                   CLEAR                   DSLV13
030900131118     C                   MOVEL     'C'           I13TLA
031000131118     C                   CALL      'FNLV13R'
031100131118     C                   PARM                    KPJBA
031200131118     C                   PARM                    DSLV13
031300131118     C                   PARM                    DSSI95
031400941114     C*
031500920506     C                   SETON                                        LR
031600941114     C*------------------------------------------------------------------------*
031700941114     C* STAMPA - STAMPA LETTERA DI VETTURA
031800941114     C*------------------------------------------------------------------------*
031900941114     C     STAMPA        BEGSR
032000941114     C*
032100920506     C* SE RISTAMPA LA PRIMA VOLTA LEGGO IL NUMERO DI BOLLA ESISTENTE
032200941114     C   15
032300941114     CANN90              Z-ADD     UNMDOC        NBOL              7 0
032400920506     C   15
032500920506     CANN90              SETON                                        90
032600941114     C*
032700920505     C* PRELEVO DATI UNITA'
032800100916     C**** KANA          CHAIN     UNANA01L                           12
032900941114     C                   MOVEL     UNADES        DESUNI           20
033000101008     c                   movel     unamag        wmag
033100100916     c                   exsr      Partecomune
033200920506     C*
033300131212     c* Se non passo da Wfile non stampo ma devo invece scrivere il fivax
033400131217     c                   if        parbol='N'  or *in15
033500920506     C                   EXCEPT    RIGA
033600920506     C                   ADD       1             CNTR              2 0
033700131212     c                   else
033800131212     c* Creo il fivax
033900131212     c                   if        flgai= 1 and not *in15
034000131212     c                   if        felluo<>046 or
034100131212     c                             (felluo=046 and §ogfiue>*zeros)
034200131212     c                   eval      wtvax='R'
034300131212     c                   exsr      sr_fivax
034400131212     c                   endif
034500131212     c                   endif
034600131213     c                   endif
034700920506     C*
034800941114     C* SE NON E' RISTAMPA AGGIORNO NUMERO DOCUMENTO SUL MOVIMENTO
034900100915    1c                   if        not *in15
035000100915     C                   Z-ADD     BOLLA         UNMDOC
035100100915     C                   EXCEPT    AGG
035200100915     c
035300100915     c* Solo per movimenti automatici
035400100915     c* Aggiorno anche UNGIO col numero della bolla e flag bolla stampata
035500100916    2c                   if        parmaut='S'
035600100915
035700100915     c     gio1nrr       chain     ungiofis
035800100916    3c                   if        %found
035900100915     c                   z-add     bolla         ungdoc
036000100915     c                   eval      ungfst='S'
036100100915     c                   update    ungiofis
036200100916    3c                   endif
036300100915     c
036400100916    2c                   endif
036500100915    1c                   endif
036600920506     C*
036700920505     C                   ENDSR
036800941114     C*------------------------------------------------------------------------*
036900941114     C* CARFBL - SCRIVE UN RECORD NELL'ARCHIVIO SPEDIZIONI
037000941114     C*------------------------------------------------------------------------*
037100921102     C     CARFBL        BEGSR
037200021030     C                   CLEAR                   FIVAB000
037300131118     C                   CLEAR                   FIVAB050
037400150213     c                   clear                   wlog0
037500080924     c* Imposto anno/mese giorno spedizione che mi serve per la pulizia
037600080924     c                   movel     dateu         vabaas
037700080924     c                   move      dateu         vabmgs
037800921102     C*
037900921102     C* CODICE BOLLA PARTENZA
038000140121     c                   if        orgfag='Y'
038100140121     C                   MOVE      '1 '          VABCBO
038200140121     c                   else
038300960104     C                   MOVE      'A '          VABCBO
038400140121     c                   endif
038500131118     C                   MOVEL     '3A'          COD
038600131118     C                   MOVEL     *BLANKS       KEY
038700131203     C                   MOVEL     vabcbo        KEY
038800131118     C     KTAB2         CHAIN     TABEL
038900131118     c                   if        %found(tabel00f)
039000131118     C                   MOVEL     TBLUNI        DS3A
039100131118     c                   else
039200131118     c                   clear                   ds3a
039300131118     c                   movel     'F'           §3atb1
039400131118     c                   endif
039500131118     c
039600921102     C* RAGIONE SOCIALE DESTINATARIO
039700960104     C                   MOVEL     RAGLUO        VABRSD
039800101005     c
039900101005     C* IMPOSTO LA LNA E ZONA 00 SIA PER DESTINATARIO SDI CHE PER BAR
040000101005     C* (PER BAR SOLO SE NON FA PARTE DELL'ARCHIVIO DI APPOGGIO)
040100101005     C     s_orgDIT      IFEQ      'SDI'
040200101005     c     wappoggio     oreq      ' '
040300101005     C                   MOVE      parmag        VABLNA
040400101005     C                   MOVE      00            VABZNC
040500101005     C                   END
040600140409     c* Se archivio di appoggio prendo lna da quest'ultimo se impostata
040700140409     c                   if        wappoggio='S' and wntr>0
040800140409     c                   move      wntr          vablna
040900140409     C                   MOVE      00            VABZNC
041000140409     c                   endif
041100100924     c
041200100924     c* Se si tratta di logistica l'indirizzo lo prendo dalla filiale Bartolini correlata
041300100930     c                   if        §ogfiue>*zeros
041400150213     c                   movel     §ogfiue       wlog0             3 0
041500150213     c     wlog0         chain     azorg01l
041600100924     c                   if        %found(azorg01l)
041700100924     c                   movel     orgind        vabind
041800100924     c                   movel     orgloc        vablod
041900100924     c                   movel     orgpro        vabprd
042000100924     c                   movel     orgcpf        vabcad
042100101005     C     s_orgDIT      IFEQ      'SDI'
042200101005     c     wappoggio     oreq      ' '
042300101005     C                   MOVE      orgfil        VABLNA
042400100924     c                   endif
042500101005     c                   endif
042600100924     c
042700100924     c                   else
042800921102     C* INDIRIZZO DESTINATARIO
042900960104     C                   MOVEL     INDLUO        VABIND
043000921102     C* CAP DESTINATARIO
043100960104     C                   MOVEL     CAPLUO        VABCAD
043200921102     C* LOCALITA' DESTINATARIO
043300960104     C                   MOVEL     LOCLUO        VABLOD
043400921102     C* PROVINCIA DESTINATARIO
043500960104     C                   MOVEL     PROLUO        VABPRD
043600100924     c                   endif
043700100924     c
043800960104     C* NATURA MERCE
043900131118     C                   MOVEL     GAI(2)        VABNAS
044000921102     C* RIFERIMENTO MITTENTE NUMERICO
044100131212     C                   Z-ADD     bolla         VABRMN
044200921102     C* FLAG FERMO DEPOSITO
044300120613     c*  solo se va alla filiale e non a un fornitore (ovvero se si utilizza
044400120613     c*  organigramma di appoggio NO fermio deposito)
044500150213     c* anche per logistica NON imposto il falg fermo deposito
044600150213     c                   if        wappoggio=' '  and (wlog0=0
044700150213     c                             or §ogntw<>'LOG')
044800960104     C                   MOVE      'S'           VABFFD
044900120613     c                   endif
045000960308     C* COD.CLIENTE MITTENTE
045100960308     C                   Z-ADD     1020002       VABCCM
045200980306     C* TIPO SPEDIZIONE
045300980519     C                   MOVE      'C'           VABTSP
045400021030     C* P.O DI GESTIONE
045500021030     C                   MOVEL     VABCCM        VABFGS
045600971024     C* NOTE
045700161128     c                   eval      VABnot = 'C.A. POC'
045800100915     c* Peso e colli
045900100915     c                   if        parpkg>0
046000100915     c                   z-add     parpkg        vabpkb
046100100915     c                   endif
046200100915     c                   if        parncl>0
046300100915     c                   z-add     parncl        vabncl
046400100915     c                   endif
046500080924     c
046600131118     C* SCRIVO IL RECORD nel file di appoggio
046700131118    1c                   if        parbol='N'
046800021030     C                   WRITE     FIVAB000
046900131118   x1c                   else
047000131118     C* LINEA  DI PARTENZA
047100131118     C                   Z-ADD     102           VABLNP
047200131118     C                   Z-ADD     102           VABFGS
047300131118     C                   EXSR      CHKCAP
047400131118
047500131118    2c                   if        vablna>0 and o95lna=vablna
047600131118     C                   MOVEL     O95ZNC        VABZNC
047700131118    2C                   END
047800131118    2c                   if        vablna=0
047900131118     C                   MOVEL     O95LNA        VABLNA
048000131118     C                   MOVEL     O95ZNC        VABZNC
048100131118    2c                   endif
048200131118     c
048300131118     C                   MOVEL     O95CTS        VABCTS
048400131118     C                   Z-ADD     01            VABNRS
048500131118     C* NUMERO SPEDIZIONE
048600131118     c* lo prendo da AZNUM con trul33r
048700131118     C                   CLEAR                   TRUL33DS
048800131118     C                   Z-ADD     103           I33CNU
048900131118     C                   Z-ADD     vablnp        I33PO1
049000131118     C                   Z-ADD     1             I33NUM
049100131118     c                   movel(p)  trul33ds      kpjbu
049200131118     c                   call      'TRUL33R'
049300131118     c                   parm                    kpjba
049400131118     c                   movel     kpjbu         trul33ds
049500131118     C                   IF        O33ERR = 0
049600131118     C                   Z-ADD     O33Nrf        vabnsp
049700131118     C                   ELSE
049800131118     C                   CLEAR                   VABNSP
049900131118     C                   ENDIF
050000131118     C
050100131118     C* RICERCO IL NUMERO SEGNACOLLI E LANCIO IL PROGRAMMA STAMPA
050200131118     C* SEGNACOLLI
050300131118     c* lo prendo da AZNUM con trul33r
050400131118     C                   CLEAR                   TRUL33DS
050500131118     C                   Z-ADD     109           I33CNU
050600131118     C                   Z-ADD     vablnp        I33PO1
050700131118     C                   Z-ADD     vabncl        I33NUM
050800131118     c                   movel(p)  trul33ds      kpjbu
050900131118     c                   call      'TRUL33R'
051000131118     c                   parm                    kpjba
051100131118     c                   movel     kpjbu         trul33ds
051200131118     C                   IF        O33ERR<> 0
051300131118     C*
051400131118     C                   Z-ADD     0             VABNCD
051500131118     C                   Z-ADD     0             VABNCA
051600131118     C                   ELSE
051700131118     c                   z-add     o33nri        vabncd
051800131118     c                   z-add     o33nrf        vabnca
051900131118     C                   END
052000131118     C*
052100131118     C* SE AL = DAL AZZERO IL CAMPO
052200131118     C     VABNCA        IFEQ      VABNCD
052300131118     C                   Z-ADD     0             VABNCA
052400131118     C                   END
052500131118     C*
052600131118     C* CODICE TRATTAMENTO MERCE che prevede stampa LDV
052700131213     C                   MOVEL     '3X'          VABCTM
052800131118     C*
052900131118     C*  WRITE
053000131118     C                   WRITE     FIVAB050
053100131118     c
053200131118     C* STAMPO I SEGNACOLLI
053300131118     C* SALVO LA FILIALE CHE STO ELABORANDO
053400131118     C                   MOVEL     ORGFIL        SAVFIL            3 0
053500131212     c                   eval      wtvax='T'
053600131212     c                   exsr      sr_fivax
053700131118     C*
053800131118     C     ENDCRE        TAG
053900131118    1C                   ENDIF
054000921102     C*
054100921102     C                   ENDSR
054200131212     C*------------------------------------------------------------------------*
054300131212     C*  PrtScnLdv - Stampa segnacolli e lettera di vettura se non passa da Wf
054400131212     C*------------------------------------------------------------------------*
054500131212     c* stampa segnacolli e lettera di vettura
054600131212     c     PrtScnLdv     begsr
054700131212     C                   EXSR      GESSNC
054800131212     C*
054900131212     C     SAVFIL        CHAIN     AZORG01L                           95
055000131212     C* STAMPA LETTERA DI VETTURA
055100131212     C                   EXSR      GESLET
055200131212     c                   endsr
055300131118     C*------------------------------------------------------------------------*
055400131118     C*  CHKCAP - CONTROLLO FORMALE DEL CAP
055500131118     C*------------------------------------------------------------------------*
055600131118     C     CHKCAP        BEGSR
055700131118     C*
055800131118     C* IMPOSTO CAMPI DS EST. E LANCIO PGM PER CONTROLLO
055900131118     C                   CLEAR                   DSSI95
056000131118     C                   CLEAR                   DSLV13
056100131118     C* CONTROLLO: 1-FORMALE  2-SU CAPPARIO 3-CONGRUENZA CON PROVINC
056200131118     C                   MOVEL     '3'           I95TCN                                     LLO FORM
056300131118     C                   MOVEL     VABCAD        I95CAP                         *CAP
056400131118     C                   MOVEL     VABPRD        I95PRV
056500131118     C                   MOVEL     VABLOD        I95LOC
056600131118     C                   Z-ADD     vabpkb        I95LKG
056700131118     C                   Z-ADD     vabvlb        I95LMC
056800131118     C                   MOVEL     VABFFD        I95FFD
056900131118     C                   Z-ADD     VABMGS        I95DAT
057000131118     C                   MOVEL     VABAAS        I95DAT
057100131118     C                   MOVEL     'S'           I13AF0
057200131118     C                   MOVEL     'S'           I13CNG
057300131118     C     §3AFCA        IFNE      0
057400131118     C                   MOVEL     'S'           I95FCA
057500131118     C                   ENDIF
057600131118     C                   MOVEL     §3ATB1        I95TPO
057700131118     C                   MOVEL     'S'           I95FRE
057800131118     C                   MOVEL     VABTSP        I95TSP
057900131118     C**
058000131118     C**  TERMINAL DI PARTENZA DELLA LNP
058100131118     C                   CLEAR                   DSLV55
058200131118     C                   MOVEL     'P'           D55TPT
058300131118     C                   MOVE      VABLNP        D55LIN
058400131118     C                   MOVE      VABMGS        D55DRF
058500131118     C                   MOVEL     VABAAS        D55DRF
058600131118     C                   CALL      'FNLV55R'
058700131118     C                   PARM                    DSLV55
058800131118     C**
058900131118     C                   MOVEL     D55TFP        I95TFP
059000131118     C*
059100131118     C                   CALL      'FNLV13R'
059200131118     C                   PARM                    KPJBA
059300131118     C                   PARM                    DSLV13
059400131118     C                   PARM                    DSSI95
059500131118     C**
059600131118     C* ERRORE
059700131203    1C**   O13ERR        IFNE      ' '
059800131118     C* MEMORIZZO SOLTANTO 11 FILIALI SBAGLIATE
059900131203     C**   X             IFLE      11
060000131203     C**                 MOVEL     ORGFIL        FER(X)
060100131203     C**                 ADD       1             X
060200131203     C**                 ENDIF
060300131118     C**
060400131203     C**                 SETON                                        90
060500131203    1C**                 ENDIF
060600131118     C*
060700131118     C                   ENDSR
060800131118     C*---------------------------------------------------------------
060900131118     C*   GESSNC  GESTIONE STAMPA SEGNACOLLI
061000131118     C*---------------------------------------------------------------
061100131118     C*
061200131118     C     GESSNC        BEGSR
061300131118     C*
061400131118     C                   Z-ADD     VABNCD        CO2SCD            7 0
061500131118     C     VABNCA        IFEQ      0
061600131118     C                   Z-ADD     VABNCD        VABNCA
061700131118     C                   END
061800131118     C*
061900131118     C* VALORIZZO LA DS PARAMETRO PASSANTE PER PGM STAMPA SEGNACOLLI
062000131118     C*
062100131118     C                   Z-ADD     0             §LDNC1
062200131118     C                   MOVEL     VABLNP        §LDLNP
062300131118     C     VABLNP        CHAIN     AZORG                              56
062400131118    1C     *IN56         IFEQ      *OFF
062500131118    2C     ORGDE5        IFNE      *BLANKS
062600131118     C                   MOVEL     ORGDE5        §LDDLP
062700131118   X2C                   ELSE
062800131118     C                   MOVEL     ORGDES        §LDDLP
062900131118    2C                   END
063000131118   X1C                   ELSE
063100131118     C                   MOVEL     *BLANKS       §LDDLP
063200131118    1C                   END
063300131118     C                   MOVEL     *BLANK        §LDCDP
063400131118     C                   MOVEL     VABNRS        §LDNRS
063500131118     C                   MOVEL     VABLNA        §LDLNA
063600131118     C     VABLNA        CHAIN     AZORG                              56
063700131118     C   56              MOVEL     *BLANKS       §LDDLA
063800131118    1C  N56ORGDE5        IFNE      *BLANKS
063900131118     C                   MOVEL     ORGDE5        §LDDLA
064000131118     C                   ELSE
064100131118     C                   MOVEL     ORGDES        §LDDLA
064200131118    1C                   END
064300131118     C                   CLEAR                   DSLV55
064400131118     C                   MOVEL     'A'           D55TPT
064500131118     C                   MOVE      VABLNA        D55LIN
064600131118     C                   MOVE      VABMGS        D55DRF
064700131118     C                   MOVEL     VABAAS        D55DRF
064800131118     C                   CALL      'FNLV55R'
064900131118     C                   PARM                    DSLV55
065000131118     C                   MOVEL     D55TFA        §LDTNA
065100131118     C                   MOVEL     VABTSP        §LDTSP
065200131118     C                   MOVEL     VABZNC        §LDZSC
065300131118     C                   MOVE      VABNCL        §LDNC2
065400131118     C* REPERISCO DATI ANAGRAIFCI CLIENTE MITTENTE
065500131118     C                   CLEAR                   DSBS69
065600131118     C                   Z-ADD     VABCCM        I69KAC
065700131118     C                   Z-ADD     VABCCM        I69KIN
065800131118     C                   MOVEL     KNSIF         I69SIF
065900131118     C                   CALL      'TIBS69R'
066000131118     C                   PARM                    DSBS69
066100131118     C                   PARM                    DSACO
066200131118     C                   PARM                    DSIND
066300131118     C                   PARM                    DSCLP
066400131118     C                   PARM                    DSCLS
066500131118     C     O69ERR        IFNE      '1'
066600131118     C     ACOFLG        IFEQ      '*'
066700131118     C                   MOVEL     *BLANKS       §LDRSM
066800131118     C                   ELSE
066900131118     C                   MOVEL     ACORAG        §LDRSM
067000131118     C                   END
067100131118     C                   MOVEL     INDPRV        §LDPRM
067200131118     C                   END
067300131118     C                   MOVEL     VABRSD        §LDRSD
067400131118     C                   MOVEL     VABCAD        §LDCAD
067500131118     C                   MOVEL     VABLOD        §LDLOD
067600131118     C                   MOVEL     VABIND        §LDIND
067700131118     C                   MOVEL     VABPRD        §LDPRD
067800131118     C                   MOVEL     999           §LDCPD
067900131118     C                   MOVEL     *ZEROS        §LDNCP
068000131118     C                   MOVEL     *ZEROS        §LDNCU
068100131118     C     VABVLB        IFGT      0
068200131118     C                   MOVEL     'R'           §LDFVR
068300131118     C                   ELSE
068400131118     C                   CLEAR                   §LDFVR
068500131118     C                   ENDIF
068600131118     C                   Z-ADD     VABVLB        §LDVOL
068700131118     C                   Z-ADD     VABPKB        §LDPKG
068800131118     C                   Z-ADD     VABAAS        §LDAAS
068900131118     C                   Z-ADD     VABMGS        §LDMGS
069000131118     C                   MOVEL     'ITA'         §DETST
069100131118     C     CO2SCD        DOWLE     VABNCA
069200131118     C                   ADD       1             §LDNC1
069300131118     C                   Z-ADD     CO2SCD        §LDNCD
069400131118     C*
069500131203     c                   Call      wd90pss
069600131118     C                   PARM                    DSBLD
069700131118     C                   PARM                    DSBLDE
069800131118     C*
069900131118     C                   ADD       1             CO2SCD
070000131118     C*
070100131118     C                   END
070200131118     C*
070300131118     C                   ENDSR
070400131118     C*---------------------------------------------------------------
070500131118     C*   GESLET  GESTIONE STAMPA LETTERA DI VETTURA
070600131118     C*---------------------------------------------------------------
070700131118     C*
070800131118     C     GESLET        BEGSR
070900131118     C                   CLEAR                   Fnlsb5ds
071000131118     C                   MOVEL     'B'           DB0GES
071100131118     C                   MOVEL     'P'           DB0TBO
071200131118     C*
071300131118     C                   Z-ADD     VABLNP        DB0LNP
071400131118     C                   Z-ADD     VABAAS        DB0AAS
071500131118     C                   Z-ADD     VABMGS        DB0MGS
071600131118     C                   Z-ADD     VABNRS        DB0NRS
071700131118     C                   Z-ADD     VABNSP        DB0NSP
071800131118     C                   MOVE      VABCBO        DB0CBO
071900131118     C                   MOVEL     §3ATB1        DB0TB1
072000131118     C                   MOVEL     §3ATB2        DB0TB2
072100131118     C                   MOVEL     §3ADES        DB0DCB
072200131118     C                   Z-ADD     VABLNA        DB0LNA
072300131118     C* PASSO INDIRIZZO COMPLETO DELLA LNA
072400131118     C     VABLNA        CHAIN     AZORG                              30
072500131118     C     *IN30         IFEQ      *OFF
072600131118     C                   MOVEL     ORGFAG        DB0FAG
072700131118     C                   MOVEL     ORGDES        DB0DLA
072800131118     C                   MOVEL     ORGIND        DB0INL
072900131118     C                   MOVEL     ORGLOC        DB0LOL
073000131118     C                   MOVEL     ORGPRO        DB0PRL
073100131118     C                   MOVEL     ORGTEL        DB0TEL
073200160519     C                   MOVEL     ORGdd2        DB0FAX
073300131118     c                   Movel     OrgCpf        Db0Cal
073400131118     C                   ENDIF
073500131118     C                   Z-ADD     VABZNC        DB0ZNC
073600131118     C* MITTENTE
073700131118     C                   MOVEL     VABCCM        DB0CCM
073800131118     C                   MOVEL     ACORAG        DB0RSM
073900131118     C                   MOVEL     INDVIA        DB0INM
074000131118     C                   MOVEL     INDCAE        DB0CAM
074100131118     C                   MOVEL     INDCIT        DB0LOM
074200131118     C                   MOVEL     INDPRV        DB0PRM
074300131118     C                   MOVEL     INDSTA        DB0NZM
074400131118     C                   MOVEL     VABRSD        DB0RSD
074500131118     C                   MOVEL     VABIND        DB0IND
074600131118     C                   MOVEL     VABLOD        DB0LOD
074700131118     C                   MOVEL     VABPRD        DB0PRD
074800131118     C                   MOVEL     VABCAD        DB0CAD
074900131118     C                   MOVEL     VABNZD        DB0NZD
075000131118     C* FLAG INOLTRO  --> SE FERMO DEPOSITO SEMPRE 'C'
075100131118     c*  ai fini della stampa il campo non serve per cui preferisco non pasarlo in vista
075200131118     c*  dei nuovi tipi inoltro per centro storico
075300131118     C                   MOVEL     VABGC1        DB0GC1
075400131118     C                   MOVEL     VABGC2        DB0GC2
075500131118     C                   MOVEL     VABTSP        DB0TSP
075600131118     C* DECODIFICA TIPO SERVIZIO
075700131118     C     VABTSP        IFNE      ' '
075800131118     C                   MOVEL     '5E'          COD
075900131118     C                   MOVEL(P)  VABTSP        KEY
076000131118     C     KTAB2         CHAIN     TABEL                              30
076100131118     C  N30              MOVEL     TBLUNI        DS5E
076200131118     C   30              CLEAR                   DS5E
076300131118     C                   MOVEL     §5EDES        DB0DTS
076400131118     C                   MOVEL     §5EFST        DB0FST
076500131118     C                   ENDIF
076600131118     C*
076700131118     C                   Z-ADD     VABIAS        DB0IAS
076800131118     C                   MOVEL     VABVAS        DB0VAS
076900131118     C                   MOVEL     VABNAS        DB0NAS
077000131118     C                   Z-ADD     VABNCL        DB0NCL
077100131118     C                   Z-ADD     VABPKB        DB0PKF
077200131118     C                   Z-ADD     VABVLB        DB0VLF
077300131118     C     DB0VLF        IFGT      0
077400131118     C                   MOVEL     'R'           DB0FVF
077500131118     C                   ENDIF
077600131118     C                   Z-ADD     VABRMN        DB0RMN
077700131118     C                   Z-ADD     VABLNP        DB0FLS
077800131118     C                   Z-ADD     VABNCD        DB0NCD
077900131118     C                   Z-ADD     VABNCA        DB0NCA
078000131118     C     DB0NCA        IFEQ      DB0NCD
078100131118     C                   CLEAR                   DB0NCA
078200131118     C                   ENDIF
078300131118     C                   MOVEL     VABXCO        DB0XCO
078400131118     C                   MOVEL     VABNOT        DB0NT1
078500131118     C                   MOVEL     VABNT2        DB0NT2
078600131118     C                   MOVEL     VABFFD        DB0FFD
078700131118     C*                                             c
078800140116     c******             call      wD90psl
078900140116     C******             PARM                    Fnlsb5ds
079000131118     C*
079100131118     C                   ENDSR
079200100916     c*------------------------------------------------------------------------*
079300100916     c* Controllo su UNGIO corrrispondenza MAG/FOR
079400100916     c*------------------------------------------------------------------------*
079500100916     C     CtrUNGIO      BEGSR
079600100916     c
079700101028     c                   clear                   ungtru
079800101028     c                   clear                   ungmag
079900101013     c                   clear                   ungcuf
080000101013     c                   clear                   gio1NRR
080100100916     c     kungio        setll     ungio01l
080200100916     c     kungio        reade     ungio01l
080300101013    2c                   dow       not %eof(ungio01l)
080400100916     c
080500101011    3c                   if        ungdtm=pardat and unmmag=ungmag
080600100916     c                   leave
080700100916     c                   else
080800100916     c                   clear                   ungcuf
080900101028     c                   clear                   ungtru
081000100916    3c                   endif
081100100916     c
081200100916     c     kungio        reade     ungio01l
081300100916    2c                   enddo
081400100916     c
081500100916     C                   ENDSR
081600100916     c*------------------------------------------------------------------------*
081700100916     c* Per movimenti automatici stampo in fondo il tipo "99"
081800100916     c*------------------------------------------------------------------------*
081900100916     C     Tipo99        BEGSR
082000100916     c     kungio3       setll     ungio03l
082100100916     c     kungio3       reade     ungio03l
082200100916    1c                   dow       not %eof(ungio03l)
082300100916     c
082400101013    2c                   if        ungtip=99  and ungcuf=parcuf
082500120525     c                             and ungfst<>'N'
082600101013     c
082700100916    3c                   if        (parris<>'S' and ungdoc=0)  or
082800100916     c                             (parris= 'S' and ungdoc>0)
082900120525     c
083000120525     c* se devo annullare imposto "N" in ungfst
083100120525   3ac                   if        parris='A'
083200120525    4c                   if        not *in15
083300120525     c                   eval      ungfst='N'
083400120525     c                   update    ungio3
083500120525    4c                   endif
083600120525     c
083700120525     c                   else
083800100916     c
083900100916     c* In ristampa prendo il numero bolla se non ancora fatto
084000100916    4c                   if        *in15 and not *in90
084100100916     C                   Z-ADD     UNgDOC        NBOL
084200100916     c                   seton                                        90
084300100916    4c                   endif
084400100916     c
084500100916     c* Stampa: parte comune a UNMOV
084600101008     c                   movel     ungmag        wmag
084700100916     c                   exsr      Partecomune
084800100916
084900100916     c* stampo riga di dettaglio UNGIO
085000131217     c                   if        parbol='N' or *in15
085100100916     c                   except    rigaUNG
085200140121     c                   else
085300140121     c* Creo il fivax
085400140121     c                   if        flgai= 1 and not *in15
085500140121     c                   if        felluo<>046 or
085600140121     c                             (felluo=046 and §ogfiue>*zeros)
085700140121     c                   eval      wtvax='9'
085800140121     c                   exsr      sr_fivax
085900140121     c                   endif
086000140121     c                   endif
086100131212     c                   endif
086200100916     c
086300100916     c* Aggiorno record UNGIO
086400100916    4c                   if        not *in15
086500100916     c                   z-add     bolla         ungdoc
086600100916     c                   eval      ungfst='S'
086700100916     c                   update    ungio3
086800100916    4c                   endif
086900120525   3ac                   endif
087000100916     c
087100100916    3c                   endif
087200100916    2c                   endif
087300100916     c
087400100916     c     kungio3       reade     ungio03l
087500100916    1c                   enddo
087600100916     c                   ENDSR
087700100916     c*------------------------------------------------------------------------*
087800100916     c* Parte comune per la stampa a unmov e ungio
087900100916     c*------------------------------------------------------------------------*
088000100916     c     Partecomune   BEGSR
088100100916     C**
088200100916     C* OPERAZIONE DA ESEGUIRE SOLO UNA VOLTA
088300100916    1C                   IF         not *in91
088400100916     c
088500100916     C* SOLO IN STAMPA PRELEVO IL NUMERO DEL C/S
088600100916    2C                   if        not *in15
088700100916     c* lo prendo da AZNUM con trul33r
088800100916     C                   CLEAR                   TRUL33DS
088900100916     C                   Z-ADD     50            I33CNU
089000100916     C                   Z-ADD     1             I33NUM
089100100916     c                   movel(p)  trul33ds      kpjbu
089200100916     c                   call      'TRUL33R'
089300100916     c                   parm                    kpjba
089400100916     c                   movel     kpjbu         trul33ds
089500100916    3C                   IF        O33ERR = 0
089600100916     C                   Z-ADD     O33Nrf        bolla             7 0
089700100916     c                   else
089800100916     c                   clear                   bolla
089900100916    3c                   endif
090000131212     c
090100131212     c* Creo il record bolla
090200131212     c                   if        flgai= 1
090300131212     c                   if        felluo<>046 or
090400131212     c                             (felluo=046 and §ogfiue>*zeros)
090500131212     C                   EXSR      CARFBL
090600131212     C                   END
090700131212     c                   endif
090800100916     c
090900100916    2C                   endif
091000100916     C**
091100131217     c                   if        parbol='N' or *in15
091200100916     C                   EXCEPT    TESTA
091300131212     c                   endif
091400100916     C                   SETON                                        91
091500100916    1C                   ENDIF
091600100916     C*
091700131217     c                   if        parbol='N'  or *in15
091800100916     C* CONTO LE RIGHE STAMPATE A SALTO PAGINA RIAZZERO
091900100916    1C     CNTR          IFGE      20
092000100916     C                   Z-ADD     0             CNTR
092100100916     C     PAGE          ADD       1             PAG
092200100916     C                   EXCEPT    PRI
092300100916     C                   EXCEPT    TESTA
092400100916     C     PAGE          SUB       1             PAG               3 0
092500100916     C                   EXCEPT    PRO
092600100916     C                   ADD       1             CNTR
092700100916    1C                   END
092800131212     c                   endif
092900100916     c
093000100916     c                   ENDSR
093100131212     c*------------------------------------------------------------------------*
093200131212     c* Scrittura del fivax
093300131212     c*------------------------------------------------------------------------*
093400131212     c     sr_fivax      BEGSR
093500131212     c                   eval      VAXATB=vabatb
093600131212     c                   eval      VAXFGS=vabfgs
093700131212     c                   eval      VAXCCM=vabccm
093800131212     c                   eval      VAXLNP=vablnp
093900131212     c                   eval      VAXAAS=vabaas
094000131212     c                   eval      VAXNRS=vabnrs
094100131212     c                   eval      VAXNSP=vabnsp
094200131212     c                   eval      VAXTMD='STD1'
094300131212     c                   select
094400131212     c* scrittura tipi record di testata: C1 e T1
094500131212     c                   when      wtvax='T'
094600131212     c                   eval      VAXTRK='C1'
094700131213     c                   eval      prg=prg+1
094800131213     c                   eval      VAXPRR=prg
094900131213     c                   eval      VAXDTT='Conto Servizio n. ' +
095000131213     c                             %editc(bolla:'Z') + '         ' +
095100131213     c                             %editW(udate:'  /  /  ')
095200131213     c                   write     fivax000
095300131212     c                   eval      VAXTRK='T1'
095400131213     c                   eval      prg=prg+1
095500131213     c                   eval      VAXPRR=prg
095600161128     c                   eval      VAXdtt = ('alla cortese attenzione del POC')
095700131213     c                   write     fivax000
095800131213     c                   eval      VAXTRK='C1'
095900131213     c                   eval      prg=prg+1
096000131213     c                   eval      VAXPRR=prg
096100131213     c                   eval      VAXDTT=('  Materiale Descrizione' +
096200131213     c                             '          ' + 'Marca           ' +
096300131213     c                             'Modello         ' + 'Matricola')
096400131213     c                   write     fivax000
096500131213     c                   eval      VAXTRK='C1'
096600131213     c                   eval      prg=prg+1
096700131213     c                   eval      VAXPRR=prg
096800131213     c                   eval      VAXDTT=('------------------------------
096900131213     c                             ----------------------------------------
097000131213     c                             ------------------------------')
097100131213     c                   write     fivax000
097200131212     c* scrittura record di riga
097300131212     c                   when      wtvax='R'
097400131212     c                   eval      VAXTRK='C1'
097500131213     c                   eval      prg=prg+1
097600131213     c                   eval      VAXPRR=prg
097700131213     c                   eval      VAXDTT='  ' + %editc(unmtip:'X') + '  ' +
097800131213     c                             %editc(unmcod:'X') + ' ' +
097900131213     c                             desuni + ' ' + unamar + ' ' + unamod +
098000131213     c                             ' ' + unamat
098100131213     c                   write     fivax000
098200140313     c* se presente codice cliente aggiungo altra riga per specificare se comodato in
098300140313     c* aumento o sostituzione + cod.cliente e ragione sociale
098400140313     c                   if        ungksc>*zeros
098500140313     c                   exsr      sr_AumSost
098600140313     c                   endif
098700131213     c* scrittura record di riga da giro 99
098800131213     c                   when      wtvax='9'
098900131213     c                   eval      VAXTRK='C1'
099000131213     c                   eval      prg=prg+1
099100131213     c                   eval      VAXPRR=prg
099200131213     c                   eval      VAXDTT='  ' + %editc(ungtip:'X') + ' Qta ' +
099300131213     c                             %editc(ungqta:'Z') + ' ' +
099400131213     c                             ungdes + '       ' + ungnot
099500131213     c                   write     fivax000
099600140313     c                   if        ungksc>*zeros
099700140313     c                   exsr      sr_AumSost
099800140313     c                   endif
099900131213     c* scrittura record di piede
100000131213     c                   when      wtvax='P'
100100131213     c                   eval      VAXTRK='C1'
100200131213     c                   eval      prg=prg+1
100300131213     c                   eval      VAXPRR=prg
100400131213     c                   eval      VAXDTT=('------------------------------
100500131213     c                             ----------------------------------------
100600131213     c                             ------------------------------')
100700131213     c                   write     fivax000
100800131213     c                   eval      VAXTRK='C1'
100900131213     c                   eval      prg=prg+1
101000131213     c                   eval      VAXPRR=prg
101100131213     c                   eval      VAXDTT=('   Colli:    ' + %editc(parncl:'2')
101200131213     c                             )
101300131213     c                   write     fivax000
101400131213     c                   eval      VAXTRK='C1'
101500131213     c                   eval      prg=prg+1
101600131213     c                   eval      VAXPRR=prg
101700131213     c                   eval      VAXDTT=('  Peso Kg:' + '  ' +
101800131213     c                             %editc(parpkg:'2'))
101900131213     c                   write     fivax000
102000131212     c                   endsl
102100131212     c                   endsr
102200140313     c*------------------------------------------------------------------------*
102300140313     c*
102400140313     c*------------------------------------------------------------------------*
102500140313     c     sr_AumSost    BEGSR
102600140313     C* REPERISCO DATI ANAGRAIFCI CLIENTE MITTENTE
102700140313     C                   CLEAR                   DSBS69
102800140313     c                   clear                   acorag
102900140313     C                   Z-ADD     ungksc        I69KAC
103000140313     C                   MOVEL     KNSIF         I69SIF
103100140313     C                   CALL      'TIBS69R'
103200140313     C                   PARM                    DSBS69
103300140313     C                   PARM                    DSACO
103400140313     C                   PARM                    DSIND
103500140313     C                   PARM                    DSCLP
103600140313     C                   PARM                    DSCLS
103700140313     c                   eval      VAXTRK='C1'
103800140313     c                   eval      prg=prg+1
103900140313     c                   eval      VAXPRR=prg
104000140313     c                   eval      VAXDTT='  Comodato in'
104100140313     c                   if        ungtru=*blanks or ungtru='A'
104200140313     c                   eval      VAXDTT=%trimr(vaxdtt) + ' aumento'
104300140313     c                   else
104400140313     c                   eval      VAXDTT=%trimr(vaxdtt) + ' sostituzione'
104500140313     c                   endif
104600140313     c                   eval      vaxdtt=%trimr(vaxdtt) + ' al cliente: '
104700140313     c                             + %editc(ungksc:'X') + ' ' + acorag
104800140313     c                   write     fivax000
104900140313     c                   endsr
105000100916     O*------------------------------------------------------------------------*
105100140207     OFNLV22P   T    LRNLR
105200100916     O* AGG - AGGIORNAMENTO NUMERO DOCUMENTO MOVIMENTI UNITA'
105300100916     O*------------------------------------------------------------------------*
105400100916     OUNMOV000  E            AGG
105500941114     O                       UNMDOC
105600941114     O*------------------------------------------------------------------------*
105700101008     OLET132    E            TESTA            06
105800120322     O                       RAGIO218            32
105900920505     O                                           54 'Conto Servizio n.'
106000920506     O              N15      BOLLA         Z     62
106100920506     O               15      NBOL          Z     62
106200941115     O                       UDATE               80 '  /  /  '
106300920504     O          E            TESTA            07
106400120322     O                       ind_218             32
106500941115     O                                           74 'Pag.'
106600941115     O                       PAGE                80
106700920504     O          E            TESTA            08
106800120322     O                       cap_218              7
106900120322     O                       loc_218           +  1
107000120322     O                       prv_218           +  1
107100920504     O          E            TESTA            09
107200941115     O                       TRA                 80
107300920504     O          E            TESTA            10
107400920504     O                                           25 'Destinatario e luogo di'
107500920504     O                                         +  1 'destinazione'
107600920504     O          E            TESTA            11
107700941115     O                       TRA                 80
107800920504     O          E            TESTA            12
107900101008     O                       wmag                10
108000960927     O                                         +  2 'P.Operativo'
108100941114     O                       ORGDES            +  1
108200920505     O          E            TESTA            13
108300971024     O                       ORGIND              44
108400941115     O          E            TESTA            14
108500971024     O                       ORGCPF              29
108600941114     O                       ORGLOC            +  1
108700941114     O                       ORGPRO            +  1
108800941114     O          E            TESTA            16
108900920505     O                                         +  2 'alla cortese attenzione'
109000161128     O                                         +  1 'del POC'
109100941114     O          E            TESTA       0  0
109200920505     O                                         +  2 'alla cortese attenzione'
109300161128     O                                         +  1 'del POC'
109400920505     O          E            TESTA            18
109500941115     O                       TRA                 80
109600920505     O          E            TESTA            19
109700920505     O                                           09 'Vettore'
109800920505     O          E            TESTA            20
109900920505     O                       RAGIO               32
110000920505     O                       SIMVIA            +  1
110100920505     O                       SIMPRV            +  1
110200920505     O          E            TESTA            21
110300941115     O                       TRA                 80
110400920505     O          E            TESTA            22
110500920505     O                                           11 'Materiale'
110600941115     O                                           23 'Descrizione'
110700941115     O                                           38 'Marca'
110800941115     O                                           56 'Modello'
110900941115     O                                           74 'Matricola'
111000920505     O          E            TESTA            23
111100941115     O                       TRA                 80
111200920505     O*
111300920505     O          E            RIGA        1
111400941115     O                       UNMTIP               4
111500941115     O                       UNMCOD              11
111600941115     O                       DESUNI              32
111700941115     O                       UNAMAR              48
111800941115     O                       UNAMOD              64
111900110407     O                       UNmMAT              80
112000100916     o
112100100916     O          E            RIGAung     1
112200100916     O                       UNgTIP               4
112300100916     o                                            8 'Qta'
112400100916     O                       UNgqta        z     11
112500110914     O                       ungdes              42
112600100916     O                       UNgnot              69
112700920506     O          E            PIEDE       2
112800941115     O                       TRA                 80
112900920506     O          E            PIEDE       1
113000920505     O                                           09 'Colli:'
113100100915     O                       parncl        2  +   4
113200971024     O               15N15   TES(1)              75
113300920506     O          E            PIEDE       1
113400971024     O               15N15   TES(2)              75
113500920508     O          E            PIEDE       1
113600920508     O                                           10 'Peso Kg:'
113700100915     O                       parpkg        2  +   2
113800920506     O          E            PIEDE       3
113900971024     O                                           60 'IMS - SEDE'
114000920506     O          E            PIEDE       2  2
114100920505     O                                           62 '______________'
114200971024     O**      E 3              PIEDE
114300971024     O**                                 25 'Ritirato il _________'
114400971024     O**      E 2              PIEDE
114500971024     O**                                 25 '_____________________'
114600920506     O          E            PRO         1
114700920506     O                                           18 'Prosegue da pag.'
114800920506     O                       PAG               +  1
114900920506     O          E            PRI         1
115000920506     O                                           62 'Segue a pag.'
115100920506     O                       PAG               +  1
115200971024**   Testo
115300920504Merci Esenti Emissione Bolla Accompagnamento
115400920504Merci ai Sensi D.P.R. 6/10/78 N. 627 Art. 4
115500921102** GAI TABELLA CONTROLLO SISTEMA INFORMATIVO
115600960105GAITRA201
115700140207MATERIALE ICT
115800100920GAITRAPSP
