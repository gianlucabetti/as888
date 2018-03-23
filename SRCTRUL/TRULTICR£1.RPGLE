000100011029     H DECEDIT('0,') DATEDIT(*DMY.)
000200060704     H* TRULTICR *---------------------------------------------------*
000300060704     H*   - CONTROLLO TUPO INCASSO E DIVISA C/A
000400060704     H*--------------------------------------------------------------*
000500060704     ftabel00f  if   e           k disk
000600060704     fazorg01l  if   e           k disk
000700100208     D MSG             S             78    DIM(13) CTDATA PERRCD(1)
000800011019
000900060704     d ds15          e ds
001000060704     d ds1a          e ds
001100060704     d dsTM          e ds
001200060704     d dsCW          e ds
001300060704     d dsCV          e ds
001400071130     d kpjba         e ds
001500060704     d trulTICDS     e ds
001600011029     d tibs02DS      e ds
001700090706     D tisit0ds      E DS
001800090706     D tisit0x1ds    E DS
001900060704     d og143         e ds
002000011030
002100060704     d §kut                                like(tblkut) inz(1)
002200060704     d §cod                                like(tblcod)
002300060704     d COD                                 like(tblcod)
002400060704     d §key                                like(tblkey)
002500060704     d KEY                                 like(tblkey)
002600011019
002700011019      *----------------------------------------------------------------
002800060704     C     KTAB          KLIST
002900060704     C                   KFLD                    §kut
003000060704     C                   KFLD                    COD
003100060704     C                   KFLD                    KEY
003200060704      *----------------------------------------------------------------
003300011019
003400011029     c     *entry        plist
003500060704     c                   parm                    trulTICDS
003600060704     c                   parm                    DS1A
003700011106
003800011019
003900060704     c                   clear                   oTICRIC
004000060704     c                   clear                   oTICTIC
004100060704     c                   clear                   oTICDIV
004200060704     c                   clear                   oTICERR
004300060704     c                   clear                   oTICMSG
004400060704     c                   clear                   oTICRIC
004500060704     C* IMPOSTAZIONI
004600060704     C                   EXSR      IMPO
004700011019
004800011019      * Se Tipo lancio 'C' --> solo chiusura file
004900060704     c                   if        iTICTla <> 'C'
005000060704     c                   exsr      controlla
005100011029     c                   endif
005200011011
005300060704     c                   if        itictla <> *blanks
005400011031     c                   eval      *inlr = *on
005500090706      *
005600090706     c                   clear                   tisit0ds
005700090706     c                   clear                   tisit0x1ds
005800090706     c                   movel     'C'           it0tla
005900090706     C                   CALL      'TISIT5R'
006000090706     C                   PARM                    tisit0ds
006100090706     C                   PARM                    tisit0x1ds
006200090706      *
006300011029     c                   else
006400011031     c                   eval      *inrt = *on
006500011029     c                   endif
006600011011
006700060704      *----------------------------------------------------
006800060704      * IMPOSAZIONI INIZIALI
006900060704      *----------------------------------------------------
007000060704     C     IMPO          BEGSR
007100060704      * Controllo validità tipo lancio
007200060704     c                   if        iTICtla <> 'C' and iTICtla <> 'L' and
007300060704     c                             iTICtla <> *blanks
007400060704     c                   clear                   iTICTLA
007500060704     c                   endif
007600060704     C
007700060704     C                   IF        ITICTLA<>'C'
007800060704     c
007900060704     C                   IF        ITICLNANTW=*Blanks
008000060704     c     iticlna       chain     azorg01l
008100060704     c                   if        %found
008200060704     C                   MOVEL     ORGDE3        OG143
008300060704     C                   MOVEL     §OGNTW        iticlnantw
008400060704     c                   endif
008500060704     c                   endif
008600060704     c
008700060704     C* PRENDO LA VALUTA DAL DESTINATARIO
008800060704     C                   MOVEL     '15'          COD
008900060704     C                   MOVEL(P)  iticnzd       KEY
009000060704     C                   EXSR      CTRTAB
009100060704     C  N32              MOVEL     TBLUNI        DS15
009200060704     C   32              CLEAR                   DS15
009300060704     C                   MOVEL     §15VLT        DESVLT            3
009400060704     c
009500060704     c                   endif
009600060704     c
009700060704     c                   ENDSR
009800011011      *----------------------------------------------------
009900060704      * controlli
010000011011      *----------------------------------------------------
010100060704     c     controlla     begsr
010200060704     C***
010300060704     c*** T I P O     I N C A S S O
010400060704     C***
010500060704     C                   MOVEL     '1A'          COD
010600060704     C* RICERCA
010700060704     C     '?'           SCAN      itictic                                90
010800060704     C*
010900060704    1C     *IN90         IFEQ      *ON
011000060704     C                   MOVEL     'T'           oticric
011100060704     C                   ELSE
011200060704     C                   MOVEL(P)  itictic       KEY
011300060704    1C                   ENDIF
011400060704     C*
011500060704     C                   EXSR      CTRTAB
011600060704     C*
011700060704    1C     oticric       IFEQ      'T'
011800060704     C                   MOVEL     §KEY          otictic
011900060705     C                   GOTO      ENDCTR
012000060704   X1C                   ELSE
012100060704     C*
012200060704    2C     *IN32         IFEQ      *ON
012300060704     C                   MOVEL     MSG(1)        oticmsg
012400060705     c                   movel     'T'           oticerr
012500060704     C                   GOTO      ENDCTR
012600060704    2C                   ENDIF
012700060704     c*
012800060704     c                   movel     itictic       otictic
012900060704    1C                   ENDIF
013000060704     C*
013100060704     C                   MOVEL     TBLUNI        DS1A
013200060705     c
013300060705     c* Se bolla partenza deve essere utilizzabile in partenza
013400090625    1c                   if        itictbo='P' and §1afg1<>'1'
013500060705     C                   MOVEL     MSG(11)       oticmsg
013600060705     c                   movel     'T'           oticerr
013700060705     C                   GOTO      ENDCTR
013800090625    1C                   ENDif
013900060704     C*
014000060704     C*  Aggancio comunque la tabella TM per verificare se abilitazioni
014100060704     c*  oppure forzature
014200060704     C                   MOVEL     'TM'          COD
014300060704     C                   MOVEL(P)  iticccm       KEY
014400060704     C*
014500060704     C                   EXSR      CTRTAB
014600060704     C  n32              MOVEL     TBLUNI        DSTM
014700060704     c   32              clear                   dstm
014800060704     C*
014900090706     c* richiamo pgm per reperimento eventuale forzatura tipo
015000090706     c* incasso per destinatario particolare
015100090706     c                   clear                   tisit0ds
015200090706     c                   clear                   tisit0x1ds
015300090706     c                   movel     'E'           it0tla
015400090706     c                   movel     iticnzd       it0naz
015500090706     c                   movel     iticprd       it0prv
015600090706     c                   movel     iticcad       it0cap
015700090706     c                   movel     iticlod       it0loc
015800090706     c                   movel     iticind       it0ind
015900090706     c                   movel     iticrsd       it0rag
016000090706     c                   call      'TISIT5R'
016100090706     c                   parm                    tisit0ds
016200090706     c                   parm                    tisit0x1ds
016300060704     C* SE devo controllare la Tabella TM dal tipo incasso,
016400060704     c*  vedo se cliente autorizzato
016500060704     c
016600090625     c* Non effettuo il controllo se richiamo da FNLS01R - imm.bolle
016700090625     c*  con ultima posizione del campo PGM = '1'
016800090625     c*  (si tratta dell'indicatore 44 ovvero del richiamo dalle
016900090625     c*   giacenze che non deve effettuare questi controlli per
017000090625     c*   creazione di bolla legata)
017100090625   1ac                   if        %subst(iticpgm:1:7)<> 'FNLS01R' or
017200090625     c                             %subst(iticpgm:10:1)<>'1'
017300090625     c
017400090625    1c                   if        §1ATTM='S'
017500060704     c*
017600060704    2C     *IN32         IFEQ      *ON
017700071130     C                   CLEAR                   tibs02ds
017800071130     c
017900071130    3c                   if        otictic ='SC'
018000071130     c                   movel     'L'           T02tla
018100071130     C                   MOVEL     'C'           T02MOD
018200071130     C                   MOVEL     'FSC'         T02COD
018300071130     c                   movel(P)  iticlna       t02ke1
018400071130     C                   CALL      'TIBS02R'
018500071130     C                   PARM                    KPJBA
018600071130     C                   PARM                    tibs02ds
018700071130    3c                   endif
018800090706    3C                   IF        ((T02ERR <>*BLANKS or t02atb<>*blanks) and
018900090706     c                              (OT0X1TICF=*blanks or OT0X1TICF<>otictic))
019000090706     c                             or (otictic<>'SC' and
019100090706     c                              (OT0X1TICF=*blanks or OT0X1TICF<>otictic))
019200100208     c* Tipo incasso "SC" inseribile in arrivo sempre solo se in bolla non c'e' alcun tipo incasso C/ASS
019300100208    4c                   if        otictic<>'SC' or itictbo='P'
019400100208    4c                                           or iticticbol<>'  '
019500100208    4c                   if        otictic<>'SC'
019600060704     C                   MOVEL     MSG(2)        oticmsg
019700100208     c                   else
019800100208     C                   MOVEL     MSG(13)       oticmsg
019900100208     c                   endif
020000091211     c                   movel     'C'           oticerr
020100060704     C                   GOTO      ENDCTR
020200100208    4C                   ENDIF
020300100208    3C                   ENDIF
020400100208     c
020500071130   x2c                   else
020600060704     C*
020700060704     C* TIC = 'TM'
020800060704     C                   SELECT
020900071130    3C     otictic       WHENEQ    'TM'
021000071130    4C     §TMFTM        IFNE      'S'
021100060704     C                   SETON                                        32
021200071130    4C                   END
021300060704     C* TIC = 'BB'
021400071130    3C     otictic       WHENEQ    'BB'
021500060704     C     otictic       OREQ      'CB'
021600071130    4C     §TMFBB        IFNE      'S'
021700060704     C     §TMFBC        OREQ      'S'
021800060704     C                   SETON                                        32
021900071130    4C                   END
022000060704     C* TIC = 'BC'
022100071130    3C     otictic       WHENEQ    'BC'
022200071130    4C     §TMFBC        IFNE      'S'
022300060704     C                   SETON                                        32
022400071130    4C                   END
022500060704     C* TIC = 'BA'
022600071130    3C     otictic       WHENEQ    'BA'
022700071130    4C     §TMFBA        IFNE      'S'
022800060704     C                   SETON                                        32
022900071130    4C                   END
023000060704     C* TIC = 'CA'
023100071130    3C     otictic       WHENEQ    'CA'
023200071130    4C     §TMFCA        IFNE      'S'
023300060704     C                   SETON                                        32
023400071130    4C                   END
023500090617     C* TIC = 'BP'
023600090617    3C     otictic       WHENEQ    'BP'
023700090617    4C     §TMFBP        IFNE      'S'
023800090617     C                   SETON                                        32
023900090617    4C                   END
024000060704     C* TIC = 'SC'
024100071130    3C     otictic       WHENEQ    'SC'
024200071130    4C     §TMFSC        IFNE      'S'
024300071130     c* Verifico anche tabella FSC  forzatura per lna
024400071130     C                   CLEAR                   tibs02ds
024500071130     c                   movel     'L'           T02tla
024600071130     C                   MOVEL     'C'           T02MOD
024700071130     C                   MOVEL     'FSC'         T02COD
024800071130     c                   movel(P)  iticlna       t02ke1
024900071130     C                   CALL      'TIBS02R'
025000071130     C                   PARM                    KPJBA
025100071130     C                   PARM                    tibs02ds
025200100205     c
025300071130    5C                   IF        T02ERR <>*BLANKS or  t02atb<>*blanks
025400100205     c
025500100205     c* Tipo incasso "SC" inseribile in arrivo sempre solo se in bolla non c'e' alcun tipo incasso C/ASS
025600100205    1c                   if        itictbo='P' or iticticbol<>'  '
025700060704     C                   SETON                                        32
025800100205    5C                   ENDif
025900100205    5C                   END
026000071130    4C                   END
026100060704     C* TIC = 'TO'
026200071130    3C     otictic       WHENEQ    'TO'
026300071130    4C     §TMFTO        IFNE      'S'
026400060704     C                   SETON                                        32
026500071130    4C                   END
026600080625     C* TIC = 'S2'
026700080625    3C     otictic       WHENEQ    'S2'
026800080625    4C     §TMFS2        IFNE      'S'
026900080625     C                   SETON                                        32
027000080625    4C                   END
027100071130    3C                   ENDSL
027200060704     C*
027300090706    4C***  *IN32         IFEQ      *ON
027400090706     c                   if        *in32 and
027500090706     c                             (OT0X1TICF=*blanks or OT0X1TICF<>otictic)
027600060704     C                   MOVEL     MSG(2)        oticmsg
027700091211     c                   movel     'C'           oticerr
027800060704     C                   GOTO      ENDCTR
027900071130    4C                   END
028000071130    2C                   END
028100060704     c
028200060704    1c                   endif
028300090625   1ac                   endif
028400060704     c
028500090625     c* Verifico se ha forzaturein manutenzione: se tipo incasso forzabile
028600060704     c*  in partenza, non modificabile
028700060704    1c                   if        §tmfti<>'  ' and otictic<>iticTICbol and
028800060704     c                             iticticbol<>*blanks
028900060704     C                   MOVEL     MSG(3)        oticmsg
029000091211     c                   movel     'C'           oticerr
029100060704     C                   GOTO      ENDCTR
029200060704    1C                   ENDif
029300090706    1c                   if        OT0X1TICF<>*blanks and itictbo='A' and
029400090706     c                             otictic<>iticticbol and
029500090706     c                             iticticbol<>*blanks
029600090706     C                   MOVEL     MSG(12)       oticmsg
029700090706     c                   movel     'T'           oticerr
029800090706     C                   GOTO      ENDCTR
029900090706    1C                   ENDif
030000060704     C***
030100060704     C***   D I V I S A
030200060704     C***
030300060704      * La divisa è obbligatoria se è inserito un importo
030400060704    1C     ITICCAS       IFGT      *ZEROS
030500060704     C     ITICdiv       ANDEQ     *BLANKS
030600060704     C                   MOVEL     MSG(4)        OTICMSG
030700060705     c                   movel     'D'           oticerr
030800060704     C                   GOTO      ENDCTR
030900060704    1C                   ENDIF
031000060704     C*
031100060704     C                   MOVEL     'CV'          COD
031200060704     C* RICERCA
031300060704     C     '?'           SCAN      iticdiv                                90
031400060704    1C     *IN90         IFEQ      *ON
031500060704     C                   MOVEL     'D'           oticric
031600060704   x1C                   ELSE
031700060704     C                   MOVEL(P)  iticdiv       KEY
031800060704    1C                   ENDIF
031900060704     C*
032000060704     C                   EXSR      CTRTAB
032100060704     C*
032200060704    1C     oticric       IFne      ' '
032300060704     C                   MOVEL     §KEY          oticdiv
032400060705     C                   GOTO      ENDCTR
032500060704   X1C                   ELSE
032600060704     C* ERRORE
032700060704    2C     *IN32         IFEQ      *ON
032800060704     C                   MOVEL     MSG(5)        oticmsg
032900060705     c                   movel     'D'           oticerr
033000060704     C                   GOTO      ENDCTR
033100060704    2C                   ENDIF
033200060704     c
033300060704     c                   movel     iticdiv       oticdiv
033400060704    1C                   ENDIF
033500060704     c
033600060704     C                   MOVEL     TBLUNI        DSCV
033700060704     C*
033800060704     C* VEDO SE AMMESSI IMPORTI DECIMALI NELL'IMPORTO
033900060704    1C     §CVFDC        IFNE      'S'
034000060704     C                   MOVE      iticcas       W0033             3 3
034100060704    2C     W0033         IFNE      0
034200060704     C                   MOVEL     MSG(6)        oticmsg
034300060705     c                   movel     'D'           oticerr
034400060704     C                   GOTO      ENDCTR
034500060704    2C                   ENDIF
034600060704    1C                   ENDIF
034700060704      *
034800060704      * Controllo il numero di decimali immessi
034900060704    1C     §CVFDC        IFEQ      'S'
035000060704     C     iticcas       ANDgt     *ZEROS
035100060704      *
035200060704    2C                   SELECT
035300060704     C     §CVDEC        WHENEQ    3
035400060704     C     §CVDEC        WHENEQ    2
035500060704     C                   MOVE      iticcas       W0010             1 0
035600060704    3C     W0010         IFNE      *ZEROS
035700060704     C                   MOVEA     MSG(7)        oticmsg
035800060704     c                   eval      %subst (oticmsg:29:2) = (%editc(§cvdec:'X'))
035900060705     c                   movel     'D'           oticerr
036000060704    3C                   ENDIF
036100060704    c
036200060704     C     §CVDEC        WHENEQ    1
036300060704     C                   MOVE      iticcas       W0020             2 0
036400060704    3C     W0020         IFNE      *ZEROS
036500060704     C                   MOVEA     MSG(7)        oticmsg
036600060704     c                   eval      %subst (oticmsg:29:2) = (%editc(§cvdec:'X'))
036700060705     c                   movel     'D'           oticerr
036800060704    3C                   ENDIF
036900060704    c
037000060704     C     §CVDEC        WHENEQ    0
037100060704    2C                   ENDSL
037200060704     c
037300060705    2c                   if        oticerr<>' '
037400060704     c                   goto      endctr
037500060704    2c                   endif
037600060704      *
037700060704    1C                   ENDIF
037800060704     C*
037900060704     C* SE DIVISA ESTERA VEDO SE POSSO ACCETTARE un TIPO INCASSO
038000060704     c*    non previsto per l'estero
038100060704    1C     §CVDVA        IFNE      'S'
038200060704     C     §1AUDE        ANDNE     'S'
038300060704     C                   MOVEL     MSG(8)        oticmsg
038400060705     c                   movel     'T'           oticerr
038500060704     C                   GOTO      ENDCTR
038600060704    2C                   END
038700060704      *
038800060704      * reperisco dati tabella 'CW'
038900060704     C                   CLEAR                   DSCW
039000060704     C                   MOVEL     'CW'          COD
039100060704     C                   MOVEL(P)  oticdiv       KEY
039200060704     C                   EXSR      CTRTAB
039300060704     C     *IN32         IFEQ      *OFF
039400060704     C                   MOVEL     TBLUNI        DSCW
039500060704     C                   ENDIF
039600060704     c
039700060704      * Controllo se la divisa immessa è valida per il tipo incasso
039800060704     C     §1ACDL        IFEQ      'S'
039900060704     C     iticdsp       ANDGT     §CWDLC
040000060704     C                   MOVEL     MSG(9)        oticmsg
040100060705     c                   movel     'D'           oticerr
040200060704     C                   GOTO      ENDCTR
040300060704     C                   ENDIF
040400060704     C*
040500060704     C* PER EXPORT BARTOLINI, DPD o FEDEX NON POSSO MODIFICARE LA DIVISA
040600060704     c*  al massimo l'eur
040700060704     C     oticdiv       IFNE      DESVLT
040800060704     C     oticdiv       ANDNE     'EUR'
040900060704     c     iticlnantw    ifeq      'EEX'
041000060704     c     iticlnantw    oreq      'DPD'
041100060704     c     iticlnantw    oreq      'FED'
041200060704     C                   MOVEL     MSG(10)       oticmsg
041300060705     c                   movel     'D'           oticerr
041400060704     C                   GOTO      ENDCTR
041500060704    2C                   ENDIF
041600060704     c                   ENDIF
041700011106
041800060704     c     ENDCTR        ENDSR
041900060704     c*
042000060704     C* RICERCA E CONTROLLO RECORD IN TABELLA ------------------------*
042100060704     C     CTRTAB        BEGSR
042200060704     C*  RICERCA
042300060704     C     oticric       IFne      ' '
042400060704     C                   MOVEL     COD           §COD
042500060704     C                   CLEAR                   §KEY
042600060704     C                   CALL      'X§TABER'
042700060704     C                   PARM                    §KUT
042800060704     C                   PARM                    §COD
042900060704     C                   PARM                    §KEY
043000060704     C                   PARM                    §DES             30
043100060704     C                   SETON                                        90
043200060704     C*
043300060704     C                   ELSE
043400060704     C*
043500060704     c* controllo
043600060704     C     KTAB          CHAIN     TABEL                              32
043700060704     C  N32TBLFLG        IFNE      ' '
043800060704     C                   SETON                                        32
043900060704     C                   END
044000060704     C                   END
044100060704     c
044200060704     C                   ENDSR
044300060704**         MSG
044400060704Tipo incasso C/Assegno errato                                                 01
044500060704Cliente non autorizzato al tipo incasso C/A indicato                          02
044600060704Tipo incasso non modificabile per questo cliente                              03
044700060704Divisa C/Assegno obbligatoria se presente importo                             04
044800060704Valuta C/Assegno errata                                                       05
044900060704Non ammessi importi con decimali per la valuta C/Assegno indicata             06
045000060704Ammessi importi con massimo __ decimali per la divisa indicata                07
045100060704Tipo incasso non utilizzabile se C/Assegno in divisa estera                   08
045200060704Tipo incasso non utilizzabile per la divisa del Contrassegno                  09
045300060704Bolla EXPORT: la divisa del C/A deve essere o quella del destinatario o "EUR" 10
045400060705Tipo Incasso C/Assegno inesistente o non e' da indicare in partenza           11
045500090706Tipo incasso non modificabile per questo destinatario                         12
045600100208Tipo incasso "SC" non ammesso per questa spedizione                           13
