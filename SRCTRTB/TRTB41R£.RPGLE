000100930317     H* TRTB41R *-----------------------------------------------------*
000200930304     H*               - GESTIONE TABELLA TESTI TARIFFE -              *
000300930304     H*---------------------------------------------------------------*
000400930303     H DECEDIT('0,') DATEDIT(*DMY/)
000500921204     FTABEL00F  UF A E           K DISK
000600921204     FAZORG01L  IF   E           K DISK
000700021024     FFNTXT01L  UF   E           K DISK
000800021024     FFNTXT02L  UF   E           K DISK    RENAME(FNTXT000:FNTXT02)
000900921204     FTRTB41D   CF   E             WORKSTN
001000021024      *-----------------------------------------------------------------
001100021024      * RIEMPIMENTO VARIABILI FORMATO VIDEO
001200921207     D COM             S             10    DIM(3) CTDATA PERRCD(1)
001300021024      *
001400940309     D PUT             S              1    DIM(50)
001500940309     D DPU             S             25    DIM(50)
001600021024      *
001700021024      * DESCRIZIONE DEL COMANDO DA PASSARE AL QCMDEXC
001800921207     D CMD             S              1    DIM(34) CTDATA PERRCD(34)
001900021024      *
002000921204     D KPJBA         E DS
002100921204     D  PROFIL                 1      3
002200021024      * DS PER LA LETTURA DI TBLUNI: CAMPO §1HFLT ALFANUMERICO
002300921204     D DS1H          E DS
002400021024     D DFNTXTH       E DS
002500021022     D* NUOFLT                64     66
002600021024      *
002700021024      * DS PER RICERCA RAGIONE SOCIALE
002800021024     D trul12DS      E DS
002900021022     D CNCR80        E DS
003000921209     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003100021022     D FNLV41DS      E DS
003200021024      *
003300021024     d rsut            s             20
003400021024     d let             s                   like(TXTLET)
003500021024     d Klin            s                   like(TXTlin)
003600021024     d Kpos            s                   like(TXTpos)
003700021024     d COD             s                   like(TBLcod)
003800021024     d KEY             s                   like(TBLkey)
003900021024
004000021024      *---------------------------------------------------------------*
004100021024      * INDICATORI USATI                                              *
004200021024      *---------------------------------------------------------------*
004300921207     C* 01 - CAMPO OBBLIGATORIO PER CODICE TESTO
004400921207     C* 02 - TESTO IN USO PER CODICE TESTO
004500921210     C* 03 - CAMPO OBBLIGATORIO TESTO LIBERO
004600921207     C* 04 - VISUALIZZAZIONE NOME PROGRAMMA PER PROFILO EDP
004700921207     C* 06 - CODICE FILIALE SOLO PER TRASMISSIONE TESTO UGUALE A DUE
004800921207     C* 07 - CAMPO OBBLIGATORIO PER VI2FLT SE VI2FFT UGUALE A DUE
004900921207     C* 08 - CODICE FILIALE ERRATO
005000921207     C* 10 - PROTEZIONE CAMPI PER RECORD TABELLA ANNULLATA
005100921207     C* 11 - INSERIRE CODICE FILIALE ELABORATORE
005200921207     C* 12 - CODICE FILIALE INESISTENTE
005300921207     C* 13 - PROGRAMMA DI STAMPA NON TROVATO
005400921209     C* 14 - NOME PROGRAMMA OBBLIGATORIO
005500921209     C* 15 - INIBIZIONE CMD3 E CMD12 PER RECORD TESTO GIA ALLOCATO
005600921209     C* 16 - TESTO IN USO DOPO TRE TENTATIVI TELEFONARE IN SEDE
005700921209     C* 17 - DESCRIZIONE TESTO OBBLIGATORIA
005800930317     C* 18 - TIPO E FILIALE DI TRASMISSIONE INVARIATI PER INVIO DATI NO
005900921207     C* 20 - ABILITA CMD5  - RIPRISTINO
006000921207     C* 21 - ABILITA CMD6  - AGGIORNAMENTO
006100921207     C* 22 - ABILITA CMD16 - ANNULLAMENTO
006200921209     C* 23 - VISUALIZZAZIONE TESTO ESISTENTE
006300921207     C* 30 - AGGANCIO TABEL RECORD NON  TROVATO
006400021022     C* 31 - LETTURA  FNTXT RECORD NON TROVATO
006500921207     C* 32 - LOKUP DEL '?' IN VI2FLT
006600021024     C* 33 - COMODO
006700921207     C* 34 - AGGANCIO AZORG ESISTENZA CODICE FILIALE
006800021022     C* 35 - AGGANCIO  FNTXT RECORD NON TROVATO
006900921209     C* 36 - TESTN SUL CODICE FILIALE PER MUOVERE ZERI SE CI SONO SPAZI
007000940309     C* 37 - LOKUP ANDATA A BUON FINE
007100921207     C* 50 - CONTROLLI SUL CODICE DELLA FILIALE
007200921207     C* 51 - PROGRAMMA DI STAMPA NON TROVATO (QCMDEXC)
007300921207     C* 52 - CALL QCMDEXC SOLO SE PROFILO EDP E NOME PGM DIVERSO
007400021022     C* 90 - LETTURA  FNTXT RECORD GIA ALLOCATO
007500021022     C* 91 - AGGIORNAMENTO FNTXT RECORD GIA ALLOCATO
007600921210     C* 92 - RECORD TABELLA GIA' ALLOCATO
007700921207     C* 99 - ERRORE GENERICO
007800021024
007900021024      * Richiamo pgm per controllo modificabilità tabella
008000021024     C                   CLEAR                   trul12DS
008100971216     C                   MOVEL     'L'           I12TLA
008200971216     C                   MOVEL     KNSIF         I12NSI
008300971216     C                   MOVEL     '1H'          I12COD
008400971216     C                   CALL      'TRUL12R'
008500021024     C                   PARM                    trul12DS
008600971216     C     O12ERR        IFEQ      '1'
008700971216     C                   GOTO      FINE
008800971216     C                   END
008900921204     C*
009000021024      * DEFINIZIONI VARIABILI E KEYLIST
009100930304     C                   EXSR      DEFVAR
009200021024      *
009300021024      * CARICO TABELLA PROFILI UTENTI PER LA GESTIONE TESTI
009400940309     C                   EXSR      CARTAB
009500940309     C*
009600940308     C     FMTD3         TAG
009700940308     C                   MOVEL     '? '          VIDKEY
009800021024     C* EMISSIONE 1° FORMATO - RICHIESTA CODICE TABELLA
009900940308     C     FMTD1         TAG
010000921204     C                   EXFMT     TB41D1
010100921204     C*
010200921204     C* CMD3 - FINE LAVORO
010300921204     C   KC              GOTO      FINE
010400921204     C*
010500921204     C* CONTROLLO DATI I FORMATO
010600930304     C                   EXSR      CTLD1
010700921204     C*
010800921204     C* SE ERRORI
010900940208     C   99              GOTO      FMTD1
011000940208     C*
011100021024     C* GESTIONE RIEMPIMENTO CAMPI 2° FORMATO
011200930304     C                   EXSR      RIED1B
011300921210     C*
011400921210     C* RECORD TESTO IN USO
011500921210     C   02              GOTO      FMTD1
011600921210     C*
011700921210     C* EMISSIONE II FORMATO - RECORD TABELLA
011800921204     C     FMTD2         TAG
011900921204     C                   EXFMT     TB41D2
012000921204     C*
012100921204     C* CMD3 - FINE LAVORO
012200921204     C   KC              GOTO      FINE
012300921207     C*
012400921207     C* CMD12 - RITORNO VIDEATA PRECEDENTE
012500940308     C   KL              GOTO      FMTD3
012600921204     C*
012700921204     C* CONTROLLO DATI II FORMATO
012800930304     C                   EXSR      CTLD2
012900921204     C*
013000921204     C* SE ERRORI
013100021024     C   99              GOTO      FMTD2
013200021024
013300021024      * CMD5 - RIPRISTINO
013400021024     c                   IF        *inKE = *ON
013500021024     C                   EXSR      RIPRI
013600021024     C   92              GOTO      FMTD2
013700021024     C                   GOTO      FMTD3
013800021024     c                   ENDIF
013900021024
014000021024      * CMD6 - AGGIORNAMENTO
014100021024     c                   IF        *inKF = *ON
014200021024     C                   EXSR      AGGIO
014300021024      *  TESTO IN USO: CMD3 E CMD12 INIBITI, MESSAGGIO DI ERRORE
014400021024     C   91              GOTO      FMTD2
014500021024     C                   GOTO      FMTD3
014600021024     C                   ENDIF
014700021024
014800021024      * CMD16 - ANNULLAMENTO
014900021024     c                   IF        *inKQ = *ON
015000021024     C                   EXSR      ANNUL
015100021024     C   92              GOTO      FMTD2
015200021024     C                   GOTO      FMTD3
015300021024     C                   ENDIF
015400921209     C*
015500921209     C* PER ENTER RIMANE NELLA SECONDA VIDEATA
015600921210IF  4C     *INKE         IFEQ      '0'
015700921209     C     *INKF         ANDEQ     '0'
015800921209     C     *INKQ         ANDEQ     '0'
015900921209     C                   GOTO      FMTD2
016000921210E   4C                   ENDIF
016100021024      *
016200921209     C     FINE          TAG
016300021024     C                   eval      *inLR = *on
016400940309     C*---------------------------------------------------------------*
016500940309     C*                    CARTAB                                     *
016600940309     C* CARICAMENTO TABELLA PROFILI UTENTI PER LA GESTIONE TESTI      *
016700940309     C*---------------------------------------------------------------*
016800940309     C     CARTAB        BEGSR
016900940309     C*
017000940309     C                   Z-ADD     1             X                 4 0
017100940309     C                   MOVEL     '1I'          COD
017200940309     C     KTAB2         SETLL     TABEL
017300940309     C     KTAB2         READE     TABEL                                  33
017400940309     C*
017500940309     C     *IN33         DOWEQ     *OFF
017600940309     C     TBLFLG        IFEQ      ' '
017700940309     C                   MOVEL     TBLKEY        PUT(X)
017800940309     C                   MOVEL     TBLUNI        DPU(X)
017900940309     C                   ADD       1             X
018000940309     C                   ENDIF
018100940309     C     KTAB2         READE     TABEL                                  33
018200940309     C                   ENDDO
018300940309     C*
018400940309     C                   ENDSR
018500940309     C*
018600930304     C*---------------------------------------------------------------*
018700930304     C*                    CTLD1                                      *
018800930304     C* ROUTINE DI CONTROLLO DATI PRIMO FORMATO VIDEO                 *
018900930304     C*---------------------------------------------------------------*
019000921210     C     CTLD1         BEGSR
019100021024      *
019200021024     c                   eval      *in30 = *on
019300021024     c                   eval      *in99 = *off
019400021024     c                   eval      *in92 = *off
019500921210     C*
019600921210     C* RICERCA IN TABELLA CODICE TESTO PER '?'
019700940308     C     '?'           SCAN      VIDKEY                                 99
019800940308     C*
019900940308IF  1C     *IN99         IFEQ      *ON
020000021022     C                   MOVEL     *BLANKS       VIDKEY
020100021022     C                   MOVEL     *BLANKS       VI2DES
020200021022      *
020300021024      * Richiamo FnLv41 in modalità ricerca testi
020400021022     C                   CLEAR                   Ds1H
020500021022     C                   CLEAR                   Fnlv41ds
020600021022     C                   MOVEL     '1'           I41MOD
020700021022     C                   CALL      'FNLV41R'
020800021022     C                   PARM                    FNLV41DS
020900021022     C     O41LET        IFNE      *blanks
021000021024     C                   MOVEL     O41LET        VIDKEY
021100021024     C                   MOVEL     O41dta        DS1H
021200021024     C                   MOVEL     §1Hdes        VI2DES
021300021022     C                   ENDIF
021400021024      *
021500021024      * SIA PER TESTO SELEZIONATO CHE NON RIEMETTO IL FORMATO VIDEO
021600921210     C                   GOTO      ENDC1
021700921210E   1C                   ENDIF
021800921210     C*
021900921210IF  1C     VIDKEY        IFNE      *BLANKS
022000921210     C*
022100021022     C                   CLEAR                   Ds1H
022200021022     C                   CLEAR                   Fnlv41ds
022300021022     C                   MOVEL     VIDKEY        I41LET
022400021028     C                   MOVEL     '4'           I41MOD
022500021022     C                   CALL      'FNLV41R'
022600021022     C                   PARM                    FNLV41DS
022700021022     C     O41ERR        IFNE      'E'
022800021022     C                   MOVEL     O41DTA        DS1H
022900021024     c                   eval      *in30 = *off
023000021022     C                   ENDIF
023100921210     C*
023200921210     C* CODICE TABELLA VUOTO, CAMPO OBBLIGATORIO
023300921210X   1C                   ELSE
023400921210     C                   SETON                                        9901
023500921210     C                   GOTO      ENDC1
023600921210E   1C                   ENDIF
023700921210     C*
023800921210     C     ENDC1         TAG
023900921210     C                   ENDSR
024000930304     C*---------------------------------------------------------------*
024100930304     C*                    RIED1B                                     *
024200930304     C* ROUTINE DI GESTIONE RIEMPIMENTO CAMPI II FORMATO VIDEO        *
024300930304     C*---------------------------------------------------------------*
024400921210     C     RIED1B        BEGSR
024500921210     C*
024600921210     C                   SETOFF                                       991020
024700921209     C                   SETOFF                                       212223
024800921210     C*
024900921207     C* RECORD ESISTENTE IN TABELLA
025000921210IF  1C     *IN30         IFEQ      '0'
025100921210     C*
025200921207     C                   MOVEL     COM(2)        VI2OPE
025300921209     C                   MOVEL     §1HDES        VI2DES
025400921207     C*
025500921207     C* RIEMPO I CAMPI A VIDEO CON QUELLI IN TABELLA
025600930304     C                   EXSR      RIEMP
025700921207     C*
025800921207     C* TABELLA ANNULLATA: ABILITO CMD5 - RIPRISTINO E PROTEGGO I CAMPI
025900021028IF  2C     O41ERR        IFEQ      'A'
026000921207     C                   SETON                                        2010
026100921207     C                   MOVEL     COM(1)        VI2OPE
026200921210X   2C                   ELSE
026300921207     C*
026400921207     C* TABELLA NON ANNULLATA: ABILITO CMD6  - INSERIMENTO/MODIFICA
026500921207     C*                        ABILITO CMD16 - ANNULLAMENTO
026600921207     C                   SETON                                        2122
026700921207     C*
026800921209     C                   MOVEL     VI2KEY        LET
026900050831     c                   if        §1html = *blanks
027000021024     C     KTXT          SETLL     FNTXT01L
027100021024     C     KTXT          READE(N)  FNTXT01L                             9031
027200921207     C*
027300021024     C* SE TESTO ESISTENTE, abilito CMD16 SOLO x RIGHE TESTO TUTTE ANNULLATE
027400921210DO  3C     *IN31         DOWEQ     '0'
027500021024      * VISUALIZZAZIONE 'TESTO ESISTENTE'
027600021024     C                   eval      *in23 = *on
027700930316     C*
027800921207     C* PER ON 90 RECORD GIA ALLOCATO
027900921210IF  4C     *IN90         IFEQ      '1'
028000021024     C                   eval      *in02 = *on
028100921210     C                   GOTO      ENDRD1
028200921210E   4C                   ENDIF
028300921207     C*
028400921210     C* PER RIGA TESTO NON ANNULLATA DISABILITO CMD16 ED ESCO DAL CICLO
028500921210IF  4C     TXTATB        IFEQ      *BLANKS
028600021024     C                   eval      *in22 = *off
028700021024     C                   eval      *in31 = *on
028800921210E   4C                   ENDIF
028900921207     C*
029000021024     C  N31KTXT          READE(N)  FNTXT01L                             9031
029100921210E   3C                   ENDDO
029200050831     c                   endif
029300921210E   2C                   ENDIF
029400921207     C*
029500930303     C* RECORD TABELLA INESISTENTE: CAMPI PULITI E CMD6 ABILITATO
029600921210X   1C                   ELSE
029700921207     C                   MOVEL     COM(3)        VI2OPE
029800921207     C                   MOVEL     *BLANKS       VI2DES
029900930304     C                   EXSR      VALOR
030000921207     C                   SETON                                        21
030100921210E   1C                   ENDIF
030200940309     C*
030300921210     C     ENDRD1        TAG
030400921207     C                   ENDSR
030500930304     C*---------------------------------------------------------------*
030600930304     C*                    RIEMP                                      *
030700930304     C* ROUTINE DI RIEMPIMENTO II FORMATO PER TABELLA 1H ESISTENTE    *
030800930304     C*---------------------------------------------------------------*
030900921207     C     RIEMP         BEGSR
031000921207     C*
031100940308     C                   SETOFF                                       04
031200921207     C*
031300921207     C                   MOVEL     VIDKEY        VI2KEY
031400930318     C*
031500930318IF  1C     §1HIPV        IFEQ      'S'
031600930318     C                   MOVEL     'SI'          VI2IPV
031700930318     C                   ELSE
031800930318     C                   MOVEL     'NO'          VI2IPV
031900930318E    C                   ENDIF
032000930318     C*
032100050912     c                   movel     §1html        vi2tml
032200060606     c                   movel     §1hlin        vi2lin
032300960624     C                   MOVEL     §1HRSR        VI2RSR
032400960624     C                   MOVEL     §1HFTT        VI2FTT
032500921209     C*
032600921209     C* CONTROLLO SE IN §1HFLT CI SONO DEI *BLANKS
032700921209     C*
032800021022     C**                 TESTN                   NUOFLT               36
032900021022IF  1C**   *IN36         IFEQ      '0'
033000021022     C**                 MOVEL     *ZEROS        NUOFLT
033100021022E   1C**                 ENDIF
033200921209     C*
033300021022     C**                 MOVEL     NUOFLT        VI2FLT
033400921207     C*
033500921207     C                   MOVEL     §1HFL1        VI2LMN
033600921209     C                   MOVEL     §1HFL2        VI2LSO
033700921207     C                   MOVEL     §1HFL3        VI2LCO
033800921207     C                   MOVEL     §1HFL4        VI2LOT
033900921207     C*
034000921207IF  1C     PROFIL        IFEQ      'EDP'
034100921207     C                   SETON                                        04
034200921207E   1C                   ENDIF
034300921209     C*
034400921209     C                   MOVEL     §1HPGS        VI2PGM
034500921207     C*
034600921207     C                   MOVEL     §1HPVA        VI2PRV
034700921207     C                   MOVEL     §1HFGE        VI2GSF
034800921209     C*
034900921207     C                   MOVEL     §1HFV1        VI2SIF
035000921207     C                   MOVEL     §1HFV2        VI2SIC
035100921207     C                   MOVEL     §1HFV3        VI2SCC
035200921207     C                   MOVEL     §1HFV4        VI2SCA
035300921207     C                   MOVEL     §1HFV5        VI2STC
035400921207     C                   MOVEL     §1HFV6        VI2SDC
035500921207     C                   MOVEL     §1HFV7        VI2SFF
035600940208     C                   MOVEL     §1HFV8        VI2SNP
035700940225     C                   MOVEL     §1HFV9        VI2SRI
035800931221     C                   MOVEL     §1HEST        VI2EST
035900940301     C                   MOVEL     §1HFGT        VI2FGT
036000940301     C                   MOVEL     §1HFTS        VI2FTS
036100940311     C                   MOVEL     §1HNCO        VI2NCO
036200921207     C*
036300940309     C* DECODIFICO IL FLAG GESTIONE TESTO
036400940309     C                   Z-ADD     1             X
036500940309     C     §1HFGT        LOOKUP    PUT(X)                                 37
036600940309     C     *IN37         IFEQ      *ON
036700940309     C                   MOVEA     DPU(X)        DE2FGT
036800940309     C                   ELSE
036900940309     C                   MOVEL     *BLANKS       DE2FGT
037000940309     C                   ENDIF
037100940309     C*
037200921207     C                   ENDSR
037300930304     C*---------------------------------------------------------------*
037400930304     C*                    VALOR                                      *
037500930304     C* ROUTINE DI RIEMPIMENTO II FORMATO PER TABELLA 1H INESISTENTE  *
037600930304     C*---------------------------------------------------------------*
037700921207     C     VALOR         BEGSR
037800940308     C                   SETOFF                                       04
037900921207     C*
038000921207     C                   MOVEL     VIDKEY        VI2KEY
038100921207     C*
038200930316     C                   MOVEL     'SI'          VI2IPV
038300960624     C                   MOVEL     *BLANKS       VI2RSR
038400960624     C                   MOVEL     *BLANKS       VI2FTT
038500021022     C**                 MOVEL     '00?'         VI2FLT
038600921207     C*
038700921207     C                   MOVEL     'S'           VI2LMN
038800921207     C                   MOVEL     'N'           VI2LSO
038900921207     C                   MOVEL     'N'           VI2LCO
039000921207     C                   MOVEL     'N'           VI2LOT
039100921207     C                   MOVEL     'S'           VI2PRV
039200921207     C                   MOVEL     'F'           VI2GSF
039300921207     C                   MOVEL     'N'           VI2SIF
039400921207     C                   MOVEL     'N'           VI2SIC
039500921207     C                   MOVEL     'N'           VI2SCC
039600921207     C                   MOVEL     'N'           VI2SCA
039700921207     C                   MOVEL     'N'           VI2STC
039800921207     C                   MOVEL     'N'           VI2SDC
039900921207     C                   MOVEL     'N'           VI2SFF
040000940208     C                   MOVEL     'N'           VI2SNP
040100940225     C                   MOVEL     'N'           VI2SRI
040200940301     C                   MOVEL     ' '           VI2FGT
040300940301     C                   MOVEL     ' '           VI2FTS
040400940208     C                   MOVEL     ' '           VI2EST
040500940311     C                   Z-ADD     1             VI2NCO
040600050831     C                   clear                   VI2tml
040700060606     c                   clear                   vi2lin
040800921207     C*
040900950316     C                   MOVEL     'FNLV45R'     VI2PGM
041000921207     C*
041100921207IF  1C     PROFIL        IFEQ      'EDP'
041200921207     C                   SETON                                        04
041300921207E   1C                   ENDIF
041400940309     C*
041500940309     C* DECODIFICO IL FLAG GESTIONE TESTO
041600940309     C                   Z-ADD     1             X
041700940309     C     VI2FGT        LOOKUP    PUT(X)                                 37
041800940309     C     *IN37         IFEQ      *ON
041900940309     C                   MOVEA     DPU(X)        DE2FGT
042000940309     C                   ELSE
042100940309     C                   MOVEL     *BLANKS       DE2FGT
042200940309     C                   ENDIF
042300921207     C*
042400921207     C                   ENDSR
042500930304     C*---------------------------------------------------------------*
042600930304     C*                    CTLD2                                      *
042700930304     C* ROUTINE DI CONTROLLO DATI SECONDO FORMATO VIDEO               *
042800930304     C*---------------------------------------------------------------*
042900921207     C     CTLD2         BEGSR
043000021024     C                   SETOFF                                       3250
043100921207     C                   SETOFF                                       515299
043200921207     C*
043300921209     C* DESCRIZIONE OBBLIGATORIA
043400921209IF  1C     VI2DES        IFEQ      *BLANKS
043500921209     C                   SETON                                        9917
043600921209     C                   GOTO      ENDC2
043700021022E   1C                   ENDIF
043800921209     C*
043900940308     C* FLAG GESTIONE TESTO
044000940308     C     VI2FGT        IFEQ      '?'
044100940308     C                   MOVEL     '1I'          COD
044200940308     C                   MOVEL     *BLANKS       VI2FGT
044300940308     C                   MOVEL     *BLANKS       DES              25
044400940308     C                   CALL      'X§TABER'
044500940308     C                   PARM                    CODUT
044600940308     C                   PARM                    COD
044700940308     C                   PARM                    KEY
044800940308     C                   PARM                    DES
044900940308     C                   MOVEL     KEY           VI2FGT
045000940308     C                   MOVEL     DES           DE2FGT
045100940308     C                   GOTO      ENDC2
045200940308     C                   ENDIF
045300940308     C*
045400940308     C* CONTROLLO VALIDITA' DEL FLAG GESTIONE TESTO
045500940309     C                   Z-ADD     1             X
045600940309     C     VI2FGT        LOOKUP    PUT(X)                                 37
045700940309     C  N37              SETON                                        4590
045800940309     C  N37              GOTO      ENDC2
045900940309     C                   MOVEA     DPU(X)        DE2FGT
046000940308     C*
046100940311     C* NUMERO COPIE TESTO
046200940311     C     VI2NCO        IFEQ      0
046300940311     C     VI2NCO        ORGT      255
046400940311     C                   SETON                                        4690
046500940311     C                   GOTO      ENDC2
046600940311     C                   ENDIF
046700021024      *
046800021024      * SE INVIO DATI VARIATI = 'NO' IL CAMPO TIPO TRASMISSIONE DEVE
046900021024      * RIMANERE INVARIATO (IN VARIAZIONE)
047000021024    1C                   IF        VI2ope = COM(2) and VI2IPV = 'NO'
047100021024     c                               and  VI2FTT <> §1HFTT
047200021024     c                   eval      *in90 = *on
047300021024     C                   eval      *in18 = *on
047400930317     C                   GOTO      ENDC2
047500930317E   1C                   ENDIF
047600921207     C*
047700921207IF  1C     PROFIL        IFEQ      'EDP'
047800921209     C*
047900921209     C* NOME PROGRAMMA OBBLIGATORIO
048000921209IF  2C     VI2PGM        IFEQ      *BLANKS
048100021024     c                   eval      *in90 = *on
048200021024     C                   eval      *in14 = *on
048300921209     C                   GOTO      ENDC2
048400921209E   2C                   ENDIF
048500921207     C*
048600921207     C* VARIAZIONE
048700921207IF  2C     VI2OPE        IFEQ      COM(2)
048800921207     C     VI2PGM        ANDNE     §1HPGS
048900021024     C                   eval      *in52 = *on
049000921207E   2C                   ENDIF
049100921207     C*
049200921207     C* INSERIMENTO
049300921207IF  2C     VI2OPE        IFEQ      COM(3)
049400021024     C                   eval      *in52 = *on
049500921207E   2C                   ENDIF
049600921207     C*
049700021024IF  2C                   IF        *IN52 = *on
049800921207     C                   MOVEA     VI2PGM        CMD(12)
049900921207     C                   MOVEL     *BLANKS       COMAND
050000921207     C                   MOVEA     CMD           COMAND
050100921207     C                   Z-ADD     34            LUNG
050200921207     C                   CALL      'QCMDEXC'                            51
050300921207     C                   PARM                    COMAND           80
050400921207     C                   PARM                    LUNG             15 5
050500921207     C*
050600921207     C* PROGRAMMA DI STAMPA NON TROVATO
050700921207IF  3C     *IN51         IFEQ      '1'
050800921207     C                   SETON                                        9913
050900921207     C                   GOTO      ENDC2
051000921207E   3C                   ENDIF
051100921207     C*
051200921207E   2C                   ENDIF
051300921207E   1C                   ENDIF
051400921207     C*
051500921207     C     ENDC2         TAG
051600921207     C                   ENDSR
051700930304     C*---------------------------------------------------------------*
051800930304     C*                    RIPRI                                      *
051900930304     C* ROUTINE DI RIPRISTINO RECORD TABELLA                          *
052000930304     C*---------------------------------------------------------------*
052100921209     C     RIPRI         BEGSR
052200921209     C*
052300940308     C                   MOVEL     '1H'          COD
052400940308     C                   MOVEL     *BLANKS       KEY
052500940308     C                   MOVEL     VIDKEY        KEY
052600940308     C     KTAB          CHAIN     TABEL                              3092
052700940308     C*
052800921209     C                   MOVEL     *BLANKS       TBLFLG
052900940308     C  N30              UPDATE    TABEL
053000921209     C*
053100921207     C                   ENDSR
053200930304     C*---------------------------------------------------------------*
053300930304     C*                    ANNUL                                      *
053400930304     C* ROUTINE DI ANNULLAMENTO RECORD TABELLA                        *
053500930304     C*---------------------------------------------------------------*
053600921209     C     ANNUL         BEGSR
053700921209     C*
053800940308     C                   MOVEL     '1H'          COD
053900940308     C                   MOVEL     *BLANKS       KEY
054000940308     C                   MOVEL     VIDKEY        KEY
054100940308     C     KTAB          CHAIN     TABEL                              3092
054200940308     C*
054300921209     C                   MOVEL     '*'           TBLFLG
054400940308     C  N30              UPDATE    TABEL
054500921209     C*
054600921209     C                   ENDSR
054700930304     C*---------------------------------------------------------------*
054800930304     C*                    AGGIO                                      *
054900930304     C* ROUTINE DI INSERIMENTO/MODIFICA RECORD TABELLA                *
055000930304     C*---------------------------------------------------------------*
055100921209     C     AGGIO         BEGSR
055200921209     C*
055300021024     C                   eval      *in15 = *off
055400921209     C*
055500921209     C                   MOVEL     VI2DES        §1HDES
055600021024      *
055700021024      * SFLEGGO TESTO PER INVIO, SOLO SE MODIFICATO TIPO DI TRASMISSIONE
055800930317IF  1C     VI2FTT        IFNE      §1HFTT
055900050831     c     §1html        andeq     *blanks
056000921209     C                   MOVEL     VIDKEY        LET
056100021024     C     KTXT          SETLL     FNTXT01L
056200021024     C     KTXT          READE     FNTXT01L                             9135
056300921209     C*
056400930317DO  2C     *IN35         DOWEQ     '0'
056500921209     C*
056600921209     C* PER ON 91 RECORD GIA ALLOCATO; CMD3 E CMD12 INIBITI E MESSAGGIO
056700930317IF  3C     *IN91         IFEQ      '1'
056800930317     C                   SETON                                        1516
056900921209     C                   GOTO      ENDAG
057000930317E   3C                   ENDIF
057100021024      *
057200921209     C                   MOVEL     VI2FTT        TXTFTT
057300921214     C                   MOVEL     *BLANKS       TXTFTR
057400921214     C                   Z-ADD     *ZEROS        TXTDTR
057500021024      *
057600021024      * AGGIORNAMENTO RIGA TESTO
057700021022     C                   UPDATE    FNTXT000
057800021024      *
057900021024     C     KTXT          READE     FNTXT01L                             9135
058000930317E   2C                   ENDDO
058100930317E   1C                   ENDIF
058200021024      *
058300940308     C                   MOVEL     '1H'          COD
058400021024     C                   MOVEL(P)  VIDKEY        KEY
058500021024      *
058600940308     C     KTAB          CHAIN     TABEL                              3092
058700940308     C   92              GOTO      ENDAG
058800021024      *
058900960624     C                   MOVEL     VI2RSR        §1HRSR
059000960624     C                   MOVEL     VI2FTT        §1HFTT
059100921214     C*
059200921209     C                   MOVEL     VI2LMN        §1HFL1
059300921209     C                   MOVEL     VI2LSO        §1HFL2
059400921209     C                   MOVEL     VI2LCO        §1HFL3
059500921209     C                   MOVEL     VI2LOT        §1HFL4
059600921209     C*
059700921209     C                   MOVEL     VI2PGM        §1HPGS
059800921209     C                   MOVEL     VI2PRV        §1HPVA
059900921209     C                   MOVEL     VI2GSF        §1HFGE
060000921209     C*
060100921209     C                   MOVEL     VI2SIF        §1HFV1
060200921209     C                   MOVEL     VI2SIC        §1HFV2
060300921209     C                   MOVEL     VI2SCC        §1HFV3
060400921209     C                   MOVEL     VI2SCA        §1HFV4
060500921209     C                   MOVEL     VI2STC        §1HFV5
060600921209     C                   MOVEL     VI2SDC        §1HFV6
060700921209     C                   MOVEL     VI2SFF        §1HFV7
060800940208     C                   MOVEL     VI2SNP        §1HFV8
060900940225     C                   MOVEL     VI2SRI        §1HFV9
061000931221     C                   MOVEL     VI2EST        §1HEST
061100940301     C                   MOVEL     VI2FGT        §1HFGT
061200940301     C                   MOVEL     VI2FTS        §1HFTS
061300940311     C                   MOVEL     VI2NCO        §1HNCO
061400930317     C*
061500930317IF  1C     VI2IPV        IFEQ      'NO'
061600930317     C                   MOVEL     'N'           §1HIPV
061700930317     C                   ELSE
061800930317     C                   MOVEL     'S'           §1HIPV
061900930317     C                   ENDIF
062000050912     c                   movel     vi2tml        §1html
062100060606     c                   movel     vi2lin        §1hlin
062200921209     C*
062300921209     C                   Z-ADD     1             TBLKUT
062400921209     C                   MOVEL     '1H'          TBLCOD
062500921209     C                   MOVEL     VI2KEY        TBLKEY
062600921209     C                   MOVEL     *BLANKS       TBLFLG
062700921209     C                   MOVEL     DS1H          TBLUNI
062800021024      * VARIAZIONE
062900921214IF  1C     *IN30         IFEQ      '0'
063000021024      *
063100021024      * SE VOGLIO INVIARE I PARAMETRI DELLA TABELLA VARIATI, SFLEGGO I
063200021024      * CAMPI PER LA TRASMISSIONE
063300930317IF  2C     VI2IPV        IFEQ      'SI'
063400930317     C                   MOVEL     *BLANKS       TBLFTR
063500930317     C                   Z-ADD     *ZEROS        TBLDTR
063600021024      *
063700021024      * AGGIORNO TBLFTT E TBLFLT SOLO SE SI PASSA DA NESSUNA O UNA
063800021024      * FILIALE A UNA O PIU FILIALI (VALORI SUPERIORI)
063900021022IF  3C     TBLFTT        IFNE      VI2FTT
064000921214     C                   MOVEL     VI2FTT        TBLFTT
064100930317E   3C                   ENDIF
064200930304     C*
064300930317     C* INVIO DATI VARIATI = 'NO'
064400930317X   2C                   ELSE
064500930317     C                   MOVEL     'T'           TBLFTR
064600021024     C                   Z-ADD     *hival        TBLDTR
064700930317E   2C                   ENDIF
064800930317     C*
064900930304X   1C                   ELSE
065000930304     C* INSERIMENTO
065100930304     C                   MOVEL     VI2FTT        TBLFTT
065200021024      *
065300021024      * INVIO DATI VARIATI = 'SI'
065400930317IF  2C     VI2IPV        IFEQ      'SI'
065500930317     C                   MOVEL     *BLANKS       TBLFTR
065600930317     C                   Z-ADD     *ZEROS        TBLDTR
065700930317X   2C                   ELSE
065800930317     C                   MOVEL     'T'           TBLFTR
065900021024     C                   Z-ADD     *hival        TBLDTR
066000930317E   2C                   ENDIF
066100021024      *
066200930304E   1C                   ENDIF
066300021024
066400021024      * VARIAZIONE
066500940308IF  1C     *IN30         IFEQ      '0'
066600921209     C                   UPDATE    TABEL
066700021024      *
066800021024      * Inoltre in variazione devo aggiornare fntxt record H, se esiste
066900050831     c                   if        §1html  = *blanks
067000021024     C                   move      'H'           KPos
067100021024     C                   move      'I'           KLin
067200021024     C     Ktxt1         chain     FnTxt02l                           21
067300021024     C  n21              do
067400021024     c                   clear                   DFNTXTH
067500021024     c                   movel     §1HDes        §TxDes
067600021024     c                   movel     §1HPva        §TxPva
067700021024     c                   movel     §1HFts        §TxFts
067800021024     c                   movel     §1HFv1        §TxSip
067900021024     c                   movel     §1HFv2        §TxSic
068000021024     c                   movel     §1HFv3        §TxScc
068100021024     c                   movel     §1HFv4        §TxSca
068200021024     c                   movel     §1HFv5        §TxStc
068300021024     c                   movel     §1HFv6        §TxSdc
068400021024     c                   movel     §1HFv7        §TxSpf
068500021024     c                   movel     §1HFv8        §TxSnp
068600021024     c                   movel     §1HFv9        §TxSri
068700021024     c                   movel(P)  DFNTXTH       TxtRig
068800021024     c                   update    FnTxt02
068900021024     C                   enddo
069000050831     c                   endif
069100021024     c
069200921209X   1C                   ELSE
069300921209     C* INSERIMENTO
069400921209     C                   WRITE     TABEL
069500921209E   1C                   ENDIF
069600921209     C*
069700921209     C     ENDAG         TAG
069800921209     C                   ENDSR
069900930304     C*---------------------------------------------------------------*
070000930304     C*                    DEFVAR                                     *
070100930304     C* ROUTINE DI DEFINIZIONE KLIST E VARIABILI                      *
070200930304     C*---------------------------------------------------------------*
070300921207     C     DEFVAR        BEGSR
070400021024      *
070500930316     C     *ENTRY        PLIST
070600930316     C                   PARM                    KPJBA
070700021024      *
070800930316     C                   Z-ADD     1             CODUT
070900930316     C                   CALL      'X§PARUT'
071000930316     C                   PARM                    UT§DSE
071100021024      *
071200021024     C                   MOVEL     RAGUT         RSUT
071300021022     C                   MOVEL     REC80         CNCR80
071400021024      *
071500021024      * AGGANCIO TABEL
071600921207     C     KTAB          KLIST
071700921207     C                   KFLD                    CODUT
071800921207     C                   KFLD                    COD
071900921207     C                   KFLD                    KEY
072000940309     C     KTAB2         KLIST
072100940309     C                   KFLD                    CODUT
072200940309     C                   KFLD                    COD
072300921207     C*
072400021024     C* AGGANCIO FNTXT01
072500921207     C     KTXT          KLIST
072600021024     C                   KFLD                    LET
072700021022     C                   KFLD                    SIMFEL
072800021024     C* AGGANCIO FNTXT02
072900021024     C     KTXT1         KLIST
073000021024     C                   KFLD                    LET
073100021024     C                   KFLD                    SIMFEL
073200021024     C                   KFLD                    KLIN
073300021024     C                   KFLD                    KPOS
073400021024      *
073500921207     C                   ENDSR
073600930304     C*---------------------------------------------------------------*
073700921207**
073800921207ANNULLATO
073900921207VARIAZIONE
074000921207IMMISSIONE
074100921207**
074200921207CHKOBJ OBJ(XXXXXXXX) OBJTYPE(*PGM)
