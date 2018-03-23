000100011029     H DECEDIT('0,') DATEDIT(*DMY.)
000200060704     H* TRULTICR *---------------------------------------------------*
000300060704     H*   - CONTROLLO TUPO INCASSO E DIVISA C/A
000400060704     H*--------------------------------------------------------------*
000500060704     ftabel00f  if   e           k disk
000600060704     fazorg01l  if   e           k disk
000700090706     D MSG             S             78    DIM(12) CTDATA PERRCD(1)
000800011019
000900060704     d ds15          e ds
001000060704     d ds1a          e ds
001100060704     d dsTM          e ds
001200060704     d dsCW          e ds
001300060704     d dsCV          e ds
001400071130     d kpjba         e ds
001500060704     d trulTICDS     e ds
001600011029     d tibs02DS      e ds
001601090706     D tisit0ds      E DS
001602090706     D tisit0x1ds    E DS
001700060704     d og143         e ds
001800011030
001900060704     d §kut                                like(tblkut) inz(1)
002000060704     d §cod                                like(tblcod)
002100060704     d COD                                 like(tblcod)
002200060704     d §key                                like(tblkey)
002300060704     d KEY                                 like(tblkey)
002400011019
002500011019      *----------------------------------------------------------------
002600060704     C     KTAB          KLIST
002700060704     C                   KFLD                    §kut
002800060704     C                   KFLD                    COD
002900060704     C                   KFLD                    KEY
003000060704      *----------------------------------------------------------------
003100011019
003200011029     c     *entry        plist
003300060704     c                   parm                    trulTICDS
003400060704     c                   parm                    DS1A
003500011106
003600011019
003700060704     c                   clear                   oTICRIC
003800060704     c                   clear                   oTICTIC
003900060704     c                   clear                   oTICDIV
004000060704     c                   clear                   oTICERR
004100060704     c                   clear                   oTICMSG
004200060704     c                   clear                   oTICRIC
004300060704     C* IMPOSTAZIONI
004400060704     C                   EXSR      IMPO
004500011019
004600011019      * Se Tipo lancio 'C' --> solo chiusura file
004700060704     c                   if        iTICTla <> 'C'
004800060704     c                   exsr      controlla
004900011029     c                   endif
005000011011
005100060704     c                   if        itictla <> *blanks
005200011031     c                   eval      *inlr = *on
005201090706      *
005202090706     c                   clear                   tisit0ds
005203090706     c                   clear                   tisit0x1ds
005204090706     c                   movel     'C'           it0tla
005205090706     C                   CALL      'TISIT5R'
005206090706     C                   PARM                    tisit0ds
005207090706     C                   PARM                    tisit0x1ds
005208090706      *
005300011029     c                   else
005400011031     c                   eval      *inrt = *on
005500011029     c                   endif
005600011011
005700060704      *----------------------------------------------------
005800060704      * IMPOSAZIONI INIZIALI
005900060704      *----------------------------------------------------
006000060704     C     IMPO          BEGSR
006100060704      * Controllo validità tipo lancio
006200060704     c                   if        iTICtla <> 'C' and iTICtla <> 'L' and
006300060704     c                             iTICtla <> *blanks
006400060704     c                   clear                   iTICTLA
006500060704     c                   endif
006600060704     C
006700060704     C                   IF        ITICTLA<>'C'
006800060704     c
006900060704     C                   IF        ITICLNANTW=*Blanks
007000060704     c     iticlna       chain     azorg01l
007100060704     c                   if        %found
007200060704     C                   MOVEL     ORGDE3        OG143
007300060704     C                   MOVEL     §OGNTW        iticlnantw
007400060704     c                   endif
007500060704     c                   endif
007600060704     c
007700060704     C* PRENDO LA VALUTA DAL DESTINATARIO
007800060704     C                   MOVEL     '15'          COD
007900060704     C                   MOVEL(P)  iticnzd       KEY
008000060704     C                   EXSR      CTRTAB
008100060704     C  N32              MOVEL     TBLUNI        DS15
008200060704     C   32              CLEAR                   DS15
008300060704     C                   MOVEL     §15VLT        DESVLT            3
008400060704     c
008500060704     c                   endif
008600060704     c
008700060704     c                   ENDSR
008800011011      *----------------------------------------------------
008900060704      * controlli
009000011011      *----------------------------------------------------
009100060704     c     controlla     begsr
009200060704     C***
009300060704     c*** T I P O     I N C A S S O
009400060704     C***
009500060704     C                   MOVEL     '1A'          COD
009600060704     C* RICERCA
009700060704     C     '?'           SCAN      itictic                                90
009800060704     C*
009900060704    1C     *IN90         IFEQ      *ON
010000060704     C                   MOVEL     'T'           oticric
010100060704     C                   ELSE
010200060704     C                   MOVEL(P)  itictic       KEY
010300060704    1C                   ENDIF
010400060704     C*
010500060704     C                   EXSR      CTRTAB
010600060704     C*
010700060704    1C     oticric       IFEQ      'T'
010800060704     C                   MOVEL     §KEY          otictic
010900060705     C                   GOTO      ENDCTR
011000060704   X1C                   ELSE
011100060704     C*
011200060704    2C     *IN32         IFEQ      *ON
011300060704     C                   MOVEL     MSG(1)        oticmsg
011400060705     c                   movel     'T'           oticerr
011500060704     C                   GOTO      ENDCTR
011600060704    2C                   ENDIF
011700060704     c*
011800060704     c                   movel     itictic       otictic
011900060704    1C                   ENDIF
012000060704     C*
012100060704     C                   MOVEL     TBLUNI        DS1A
012200060705     c
012300060705     c* Se bolla partenza deve essere utilizzabile in partenza
012400090625    1c                   if        itictbo='P' and §1afg1<>'1'
012500060705     C                   MOVEL     MSG(11)       oticmsg
012600060705     c                   movel     'T'           oticerr
012700060705     C                   GOTO      ENDCTR
012800090625    1C                   ENDif
012900060704     C*
013000060704     C*  Aggancio comunque la tabella TM per verificare se abilitazioni
013100060704     c*  oppure forzature
013200060704     C                   MOVEL     'TM'          COD
013300060704     C                   MOVEL(P)  iticccm       KEY
013400060704     C*
013500060704     C                   EXSR      CTRTAB
013600060704     C  n32              MOVEL     TBLUNI        DSTM
013700060704     c   32              clear                   dstm
013800060704     C*
013801090706     c* richiamo pgm per reperimento eventuale forzatura tipo
013802090706     c* incasso per destinatario particolare
013803090706     c                   clear                   tisit0ds
013804090706     c                   clear                   tisit0x1ds
013805090706     c                   movel     'E'           it0tla
013806090706     c                   movel     iticnzd       it0naz
013807090706     c                   movel     iticprd       it0prv
013808090706     c                   movel     iticcad       it0cap
013809090706     c                   movel     iticlod       it0loc
013810090706     c                   movel     iticind       it0ind
013811090706     c                   movel     iticrsd       it0rag
013812090706     c                   call      'TISIT5R'
013813090706     c                   parm                    tisit0ds
013814090706     c                   parm                    tisit0x1ds
013900060704     C* SE devo controllare la Tabella TM dal tipo incasso,
014000060704     c*  vedo se cliente autorizzato
014100060704     c
014200090625     c* Non effettuo il controllo se richiamo da FNLS01R - imm.bolle
014300090625     c*  con ultima posizione del campo PGM = '1'
014400090625     c*  (si tratta dell'indicatore 44 ovvero del richiamo dalle
014500090625     c*   giacenze che non deve effettuare questi controlli per
014600090625     c*   creazione di bolla legata)
014700090625   1ac                   if        %subst(iticpgm:1:7)<> 'FNLS01R' or
014800090625     c                             %subst(iticpgm:10:1)<>'1'
014900090625     c
015000090625    1c                   if        §1ATTM='S'
015100060704     c*
015200060704    2C     *IN32         IFEQ      *ON
015300071130     C                   CLEAR                   tibs02ds
015400071130     c
015500071130    3c                   if        otictic ='SC'
015600071130     c                   movel     'L'           T02tla
015700071130     C                   MOVEL     'C'           T02MOD
015800071130     C                   MOVEL     'FSC'         T02COD
015900071130     c                   movel(P)  iticlna       t02ke1
016000071130     C                   CALL      'TIBS02R'
016100071130     C                   PARM                    KPJBA
016200071130     C                   PARM                    tibs02ds
016300071130    3c                   endif
016400090706    3C                   IF        ((T02ERR <>*BLANKS or t02atb<>*blanks) and
016401090706     c                              (OT0X1TICF=*blanks or OT0X1TICF<>otictic))
016500090706     c                             or (otictic<>'SC' and
016502090706     c                              (OT0X1TICF=*blanks or OT0X1TICF<>otictic))
016600060704     C                   MOVEL     MSG(2)        oticmsg
016700091211     c                   movel     'C'           oticerr
016800060704     C                   GOTO      ENDCTR
016900071130    3C                   ENDIF
017000071130   x2c                   else
017100060704     C*
017200060704     C* TIC = 'TM'
017300060704     C                   SELECT
017400071130    3C     otictic       WHENEQ    'TM'
017500071130    4C     §TMFTM        IFNE      'S'
017600060704     C                   SETON                                        32
017700071130    4C                   END
017800060704     C* TIC = 'BB'
017900071130    3C     otictic       WHENEQ    'BB'
018000060704     C     otictic       OREQ      'CB'
018100071130    4C     §TMFBB        IFNE      'S'
018200060704     C     §TMFBC        OREQ      'S'
018300060704     C                   SETON                                        32
018400071130    4C                   END
018500060704     C* TIC = 'BC'
018600071130    3C     otictic       WHENEQ    'BC'
018700071130    4C     §TMFBC        IFNE      'S'
018800060704     C                   SETON                                        32
018900071130    4C                   END
019000060704     C* TIC = 'BA'
019100071130    3C     otictic       WHENEQ    'BA'
019200071130    4C     §TMFBA        IFNE      'S'
019300060704     C                   SETON                                        32
019400071130    4C                   END
019500060704     C* TIC = 'CA'
019600071130    3C     otictic       WHENEQ    'CA'
019700071130    4C     §TMFCA        IFNE      'S'
019800060704     C                   SETON                                        32
019900071130    4C                   END
020000090617     C* TIC = 'BP'
020100090617    3C     otictic       WHENEQ    'BP'
020200090617    4C     §TMFBP        IFNE      'S'
020300090617     C                   SETON                                        32
020400090617    4C                   END
020500060704     C* TIC = 'SC'
020600071130    3C     otictic       WHENEQ    'SC'
020700071130    4C     §TMFSC        IFNE      'S'
020800071130     c* Verifico anche tabella FSC  forzatura per lna
020900071130     C                   CLEAR                   tibs02ds
021000071130     c                   movel     'L'           T02tla
021100071130     C                   MOVEL     'C'           T02MOD
021200071130     C                   MOVEL     'FSC'         T02COD
021300071130     c                   movel(P)  iticlna       t02ke1
021400071130     C                   CALL      'TIBS02R'
021500071130     C                   PARM                    KPJBA
021600071130     C                   PARM                    tibs02ds
021700071130    5C                   IF        T02ERR <>*BLANKS or  t02atb<>*blanks
021800060704     C                   SETON                                        32
021900071130    5C                   END
022000071130    4C                   END
022100060704     C* TIC = 'TO'
022200071130    3C     otictic       WHENEQ    'TO'
022300071130    4C     §TMFTO        IFNE      'S'
022400060704     C                   SETON                                        32
022500071130    4C                   END
022600080625     C* TIC = 'S2'
022700080625    3C     otictic       WHENEQ    'S2'
022800080625    4C     §TMFS2        IFNE      'S'
022900080625     C                   SETON                                        32
023000080625    4C                   END
023100071130    3C                   ENDSL
023200060704     C*
023300090706    4C***  *IN32         IFEQ      *ON
023301090706     c                   if        *in32 and
023302090706     c                             (OT0X1TICF=*blanks or OT0X1TICF<>otictic)
023400060704     C                   MOVEL     MSG(2)        oticmsg
023500091211     c                   movel     'C'           oticerr
023600060704     C                   GOTO      ENDCTR
023700071130    4C                   END
023800071130    2C                   END
023900060704     c
024000060704    1c                   endif
024100090625   1ac                   endif
024200060704     c
024300090625     c* Verifico se ha forzaturein manutenzione: se tipo incasso forzabile
024400060704     c*  in partenza, non modificabile
024500060704    1c                   if        §tmfti<>'  ' and otictic<>iticTICbol and
024600060704     c                             iticticbol<>*blanks
024700060704     C                   MOVEL     MSG(3)        oticmsg
024800091211     c                   movel     'C'           oticerr
024900060704     C                   GOTO      ENDCTR
025000060704    1C                   ENDif
025001090706    1c                   if        OT0X1TICF<>*blanks and itictbo='A' and
025002090706     c                             otictic<>iticticbol and
025003090706     c                             iticticbol<>*blanks
025004090706     C                   MOVEL     MSG(12)       oticmsg
025005090706     c                   movel     'T'           oticerr
025006090706     C                   GOTO      ENDCTR
025007090706    1C                   ENDif
025100060704     C***
025200060704     C***   D I V I S A
025300060704     C***
025400060704      * La divisa è obbligatoria se è inserito un importo
025500060704    1C     ITICCAS       IFGT      *ZEROS
025600060704     C     ITICdiv       ANDEQ     *BLANKS
025700060704     C                   MOVEL     MSG(4)        OTICMSG
025800060705     c                   movel     'D'           oticerr
025900060704     C                   GOTO      ENDCTR
026000060704    1C                   ENDIF
026100060704     C*
026200060704     C                   MOVEL     'CV'          COD
026300060704     C* RICERCA
026400060704     C     '?'           SCAN      iticdiv                                90
026500060704    1C     *IN90         IFEQ      *ON
026600060704     C                   MOVEL     'D'           oticric
026700060704   x1C                   ELSE
026800060704     C                   MOVEL(P)  iticdiv       KEY
026900060704    1C                   ENDIF
027000060704     C*
027100060704     C                   EXSR      CTRTAB
027200060704     C*
027300060704    1C     oticric       IFne      ' '
027400060704     C                   MOVEL     §KEY          oticdiv
027500060705     C                   GOTO      ENDCTR
027600060704   X1C                   ELSE
027700060704     C* ERRORE
027800060704    2C     *IN32         IFEQ      *ON
027900060704     C                   MOVEL     MSG(5)        oticmsg
028000060705     c                   movel     'D'           oticerr
028100060704     C                   GOTO      ENDCTR
028200060704    2C                   ENDIF
028300060704     c
028400060704     c                   movel     iticdiv       oticdiv
028500060704    1C                   ENDIF
028600060704     c
028700060704     C                   MOVEL     TBLUNI        DSCV
028800060704     C*
028900060704     C* VEDO SE AMMESSI IMPORTI DECIMALI NELL'IMPORTO
029000060704    1C     §CVFDC        IFNE      'S'
029100060704     C                   MOVE      iticcas       W0033             3 3
029200060704    2C     W0033         IFNE      0
029300060704     C                   MOVEL     MSG(6)        oticmsg
029400060705     c                   movel     'D'           oticerr
029500060704     C                   GOTO      ENDCTR
029600060704    2C                   ENDIF
029700060704    1C                   ENDIF
029800060704      *
029900060704      * Controllo il numero di decimali immessi
030000060704    1C     §CVFDC        IFEQ      'S'
030100060704     C     iticcas       ANDgt     *ZEROS
030200060704      *
030300060704    2C                   SELECT
030400060704     C     §CVDEC        WHENEQ    3
030500060704     C     §CVDEC        WHENEQ    2
030600060704     C                   MOVE      iticcas       W0010             1 0
030700060704    3C     W0010         IFNE      *ZEROS
030800060704     C                   MOVEA     MSG(7)        oticmsg
030900060704     c                   eval      %subst (oticmsg:29:2) = (%editc(§cvdec:'X'))
031000060705     c                   movel     'D'           oticerr
031100060704    3C                   ENDIF
031200060704    c
031300060704     C     §CVDEC        WHENEQ    1
031400060704     C                   MOVE      iticcas       W0020             2 0
031500060704    3C     W0020         IFNE      *ZEROS
031600060704     C                   MOVEA     MSG(7)        oticmsg
031700060704     c                   eval      %subst (oticmsg:29:2) = (%editc(§cvdec:'X'))
031800060705     c                   movel     'D'           oticerr
031900060704    3C                   ENDIF
032000060704    c
032100060704     C     §CVDEC        WHENEQ    0
032200060704    2C                   ENDSL
032300060704     c
032400060705    2c                   if        oticerr<>' '
032500060704     c                   goto      endctr
032600060704    2c                   endif
032700060704      *
032800060704    1C                   ENDIF
032900060704     C*
033000060704     C* SE DIVISA ESTERA VEDO SE POSSO ACCETTARE un TIPO INCASSO
033100060704     c*    non previsto per l'estero
033200060704    1C     §CVDVA        IFNE      'S'
033300060704     C     §1AUDE        ANDNE     'S'
033400060704     C                   MOVEL     MSG(8)        oticmsg
033500060705     c                   movel     'T'           oticerr
033600060704     C                   GOTO      ENDCTR
033700060704    2C                   END
033800060704      *
033900060704      * reperisco dati tabella 'CW'
034000060704     C                   CLEAR                   DSCW
034100060704     C                   MOVEL     'CW'          COD
034200060704     C                   MOVEL(P)  oticdiv       KEY
034300060704     C                   EXSR      CTRTAB
034400060704     C     *IN32         IFEQ      *OFF
034500060704     C                   MOVEL     TBLUNI        DSCW
034600060704     C                   ENDIF
034700060704     c
034800060704      * Controllo se la divisa immessa è valida per il tipo incasso
034900060704     C     §1ACDL        IFEQ      'S'
035000060704     C     iticdsp       ANDGT     §CWDLC
035100060704     C                   MOVEL     MSG(9)        oticmsg
035200060705     c                   movel     'D'           oticerr
035300060704     C                   GOTO      ENDCTR
035400060704     C                   ENDIF
035500060704     C*
035600060704     C* PER EXPORT BARTOLINI, DPD o FEDEX NON POSSO MODIFICARE LA DIVISA
035700060704     c*  al massimo l'eur
035800060704     C     oticdiv       IFNE      DESVLT
035900060704     C     oticdiv       ANDNE     'EUR'
036000060704     c     iticlnantw    ifeq      'EEX'
036100060704     c     iticlnantw    oreq      'DPD'
036200060704     c     iticlnantw    oreq      'FED'
036300060704     C                   MOVEL     MSG(10)       oticmsg
036400060705     c                   movel     'D'           oticerr
036500060704     C                   GOTO      ENDCTR
036600060704    2C                   ENDIF
036700060704     c                   ENDIF
036800011106
036900060704     c     ENDCTR        ENDSR
037000060704     c*
037100060704     C* RICERCA E CONTROLLO RECORD IN TABELLA ------------------------*
037200060704     C     CTRTAB        BEGSR
037300060704     C*  RICERCA
037400060704     C     oticric       IFne      ' '
037500060704     C                   MOVEL     COD           §COD
037600060704     C                   CLEAR                   §KEY
037700060704     C                   CALL      'X§TABER'
037800060704     C                   PARM                    §KUT
037900060704     C                   PARM                    §COD
038000060704     C                   PARM                    §KEY
038100060704     C                   PARM                    §DES             30
038200060704     C                   SETON                                        90
038300060704     C*
038400060704     C                   ELSE
038500060704     C*
038600060704     c* controllo
038700060704     C     KTAB          CHAIN     TABEL                              32
038800060704     C  N32TBLFLG        IFNE      ' '
038900060704     C                   SETON                                        32
039000060704     C                   END
039100060704     C                   END
039200060704     c
039300060704     C                   ENDSR
039400060704**         MSG
039500060704Tipo incasso C/Assegno errato                                                 01
039600060704Cliente non autorizzato al tipo incasso C/A indicato                          02
039700060704Tipo incasso non modificabile per questo cliente                              03
039800060704Divisa C/Assegno obbligatoria se presente importo                             04
039900060704Valuta C/Assegno errata                                                       05
040000060704Non ammessi importi con decimali per la valuta C/Assegno indicata             06
040100060704Ammessi importi con massimo __ decimali per la divisa indicata                07
040200060704Tipo incasso non utilizzabile se C/Assegno in divisa estera                   08
040300060704Tipo incasso non utilizzabile per la divisa del Contrassegno                  09
040400060704Bolla EXPORT: la divisa del C/A deve essere o quella del destinatario o "EUR" 10
040500060705Tipo Incasso C/Assegno inesistente o non e' da indicare in partenza           11
040600090706Tipo incasso non modificabile per questo destinatario                         12
