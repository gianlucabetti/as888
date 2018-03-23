000100011029     H DECEDIT('0,') DATEDIT(*DMY.)
000200060704     H* TRULTICR *---------------------------------------------------*
000300060704     H*   - CONTROLLO TUPO INCASSO E DIVISA C/A
000400060704     H*--------------------------------------------------------------*
000500060704     ftabel00f  if   e           k disk
000600060704     fazorg01l  if   e           k disk
000700121022     D MSG             S             78    DIM(14) CTDATA PERRCD(1)
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
017100120320     c* 20/03/2012 : Se richiamato da fnls01r controllo sempre su tabella "TM"
017200120320     c*              se filiale
017300120326   1ac***                if        %subst(iticpgm:1:7)<> 'FNLS01R' or
017400120326     c***                          %subst(iticpgm:10:1)<>'1' or
017500120326     c***                          (%subst(iticpgm:1:7)='FNLS01R' and iticlnp=5)
017600090625    1c                   if        §1ATTM='S'
017700060704     c*
017800060704    2C     *IN32         IFEQ      *ON
017900071130     C                   CLEAR                   tibs02ds
018000071130     c
018100071130    3c                   if        otictic ='SC'
018200071130     c                   movel     'L'           T02tla
018300071130     C                   MOVEL     'C'           T02MOD
018400071130     C                   MOVEL     'FSC'         T02COD
018500071130     c                   movel(P)  iticlna       t02ke1
018600071130     C                   CALL      'TIBS02R'
018700071130     C                   PARM                    KPJBA
018800071130     C                   PARM                    tibs02ds
018900071130    3c                   endif
019000090706    3C                   IF        ((T02ERR <>*BLANKS or t02atb<>*blanks) and
019100120801     c                               (OT0X1TICF=*blanks or OT0X1TICF<>otictic
019200120801     c                                or OT0ERR<>' '))
019300090706     c                             or (otictic<>'SC' and
019400120801     c                              (OT0X1TICF=*blanks or OT0X1TICF<>otictic
019500120801     c                                or OT0ERR<>' '))
019600100208     c* Tipo incasso "SC" inseribile in arrivo sempre solo se in bolla non c'e' alcun tipo incasso C/ASS
019700100208    4c                   if        otictic<>'SC' or itictbo='P'
019800111028    4c                                           or (iticticbol<>'  '
019900111028     c                                           and iticticbol<>'SC')
020000100208    4c                   if        otictic<>'SC'
020100060704     C                   MOVEL     MSG(2)        oticmsg
020200100208     c                   else
020300100208     C                   MOVEL     MSG(13)       oticmsg
020400100208     c                   endif
020500091211     c                   movel     'C'           oticerr
020600060704     C                   GOTO      ENDCTR
020700100208    4C                   ENDIF
020800100208    3C                   ENDIF
020900100208     c
021000071130   x2c                   else
021100060704     C*
021200060704     C* TIC = 'TM'
021300060704     C                   SELECT
021400071130    3C     otictic       WHENEQ    'TM'
021500071130    4C     §TMFTM        IFNE      'S'
021600060704     C                   SETON                                        32
021700071130    4C                   END
021800060704     C* TIC = 'BB'
021900071130    3C     otictic       WHENEQ    'BB'
022000060704     C     otictic       OREQ      'CB'
022100071130    4C     §TMFBB        IFNE      'S'
022200060704     C     §TMFBC        OREQ      'S'
022300060704     C                   SETON                                        32
022400071130    4C                   END
022500060704     C* TIC = 'BC'
022600071130    3C     otictic       WHENEQ    'BC'
022700071130    4C     §TMFBC        IFNE      'S'
022800060704     C                   SETON                                        32
022900071130    4C                   END
023000060704     C* TIC = 'BA'
023100071130    3C     otictic       WHENEQ    'BA'
023200071130    4C     §TMFBA        IFNE      'S'
023300060704     C                   SETON                                        32
023400071130    4C                   END
023500060704     C* TIC = 'CA'
023600071130    3C     otictic       WHENEQ    'CA'
023700071130    4C     §TMFCA        IFNE      'S'
023800060704     C                   SETON                                        32
023900071130    4C                   END
024000090617     C* TIC = 'BP'
024100090617    3C     otictic       WHENEQ    'BP'
024200090617    4C     §TMFBP        IFNE      'S'
024300090617     C                   SETON                                        32
024400090617    4C                   END
024500150520     C* TIC = 'TP'
024600150520    3C     otictic       WHENEQ    'TP'
024700150520    4C     §TMFTP        IFNE      'S'
024800150520     C                   SETON                                        32
024900150520    4C                   END
025000060704     C* TIC = 'SC'
025100071130    3C     otictic       WHENEQ    'SC'
025200071130    4C     §TMFSC        IFNE      'S'
025300071130     c* Verifico anche tabella FSC  forzatura per lna
025400071130     C                   CLEAR                   tibs02ds
025500071130     c                   movel     'L'           T02tla
025600071130     C                   MOVEL     'C'           T02MOD
025700071130     C                   MOVEL     'FSC'         T02COD
025800071130     c                   movel(P)  iticlna       t02ke1
025900071130     C                   CALL      'TIBS02R'
026000071130     C                   PARM                    KPJBA
026100071130     C                   PARM                    tibs02ds
026200100205     c
026300071130    5C                   IF        T02ERR <>*BLANKS or  t02atb<>*blanks
026400100205     c
026500100205     c* Tipo incasso "SC" inseribile in arrivo sempre solo se in bolla non c'e' alcun tipo incasso C/ASS
026600100205    1c                   if        itictbo='P' or iticticbol<>'  '
026700060704     C                   SETON                                        32
026800100205    5C                   ENDif
026900100205    5C                   END
027000071130    4C                   END
027100060704     C* TIC = 'TO'
027200071130    3C     otictic       WHENEQ    'TO'
027300071130    4C     §TMFTO        IFNE      'S'
027400060704     C                   SETON                                        32
027500071130    4C                   END
027600080625     C* TIC = 'S2'
027700080625    3C     otictic       WHENEQ    'S2'
027800080625    4C     §TMFS2        IFNE      'S'
027900080625     C                   SETON                                        32
028000080625    4C                   END
028100071130    3C                   ENDSL
028200060704     C*
028300090706    4C***  *IN32         IFEQ      *ON
028400090706     c                   if        *in32 and
028500120802     c                             (OT0X1TICF=*blanks or OT0X1TICF<>otictic
028600120802     c                             or OT0ERR<>' ')
028700060704     C                   MOVEL     MSG(2)        oticmsg
028800091211     c                   movel     'C'           oticerr
028900060704     C                   GOTO      ENDCTR
029000071130    4C                   END
029100071130    2C                   END
029200060704     c
029300060704    1c                   endif
029400120326   1ac******             endif
029500121022     c*
029600121022     c* Se spedizione export controllo che il tipo incasso sia gestito dal partner
029700121022     c                   if        (iticlnantw='DPD' or iticlnantw='FED' or
029800121022     c                                 iticlnantw='EEX')
029900121022     c                             and %subst(iticpgm:1:7)='FNLS01R'
030000121022     C                   CLEAR                   tibs02ds
030100121022     c                   movel     'L'           T02tla
030200121022     C                   MOVEL     'C'           T02MOD
030300121022     C                   MOVEL     '1AX'         T02COD
030400121022     c                   movel(P)  iticlna       t02ke1
030500121022     c                   movel(P)  otictic       t02ke2
030600121022     C                   CALL      'TIBS02R'
030700121022     C                   PARM                    KPJBA
030800121022     C                   PARM                    tibs02ds
030900121022     c                   if        t02err<>*blanks or t02atb<>*blanks
031000121022     C                   MOVEL     MSG(14)       oticmsg
031100121023     c                   eval      oticmsg=%trim(oticmsg)+' ' +
031200121023     c                                     %editc(iticlna:'X')
031300121022     c                   movel     'C'           oticerr
031400121022     c                   endif
031500121022     c                   endif
031600060704     c
031700120802     c* Verifico se ha forzature in manutenzione: se tipo incasso forzabile
031800060704     c*  in partenza, non modificabile
031900060704    1c                   if        §tmfti<>'  ' and otictic<>iticTICbol and
032000060704     c                             iticticbol<>*blanks
032100121022     c                             and iticlnantw<>'DPD'
032200121022     c                             and iticlnantw<>'EEX'
032300121022     c                             and iticlnantw<>'FED'
032400060704     C                   MOVEL     MSG(3)        oticmsg
032500091211     c                   movel     'C'           oticerr
032600060704     C                   GOTO      ENDCTR
032700060704    1C                   ENDif
032800120802     c                   if        ot0err=' ' and
032900120802    1c                             OT0X1TICF<>*blanks and itictbo='A' and
033000090706     c                             otictic<>iticticbol and
033100090706     c                             iticticbol<>*blanks
033200121022     c                             and iticlnantw<>'DPD'
033300121022     c                             and iticlnantw<>'EEX'
033400121022     c                             and iticlnantw<>'FED'
033500090706     C                   MOVEL     MSG(12)       oticmsg
033600090706     c                   movel     'T'           oticerr
033700090706     C                   GOTO      ENDCTR
033800090706    1C                   ENDif
033900060704     C***
034000060704     C***   D I V I S A
034100060704     C***
034200060704      * La divisa è obbligatoria se è inserito un importo
034300060704    1C     ITICCAS       IFGT      *ZEROS
034400060704     C     ITICdiv       ANDEQ     *BLANKS
034500060704     C                   MOVEL     MSG(4)        OTICMSG
034600060705     c                   movel     'D'           oticerr
034700060704     C                   GOTO      ENDCTR
034800060704    1C                   ENDIF
034900060704     C*
035000060704     C                   MOVEL     'CV'          COD
035100060704     C* RICERCA
035200060704     C     '?'           SCAN      iticdiv                                90
035300060704    1C     *IN90         IFEQ      *ON
035400060704     C                   MOVEL     'D'           oticric
035500060704   x1C                   ELSE
035600060704     C                   MOVEL(P)  iticdiv       KEY
035700060704    1C                   ENDIF
035800060704     C*
035900060704     C                   EXSR      CTRTAB
036000060704     C*
036100060704    1C     oticric       IFne      ' '
036200060704     C                   MOVEL     §KEY          oticdiv
036300060705     C                   GOTO      ENDCTR
036400060704   X1C                   ELSE
036500060704     C* ERRORE
036600060704    2C     *IN32         IFEQ      *ON
036700060704     C                   MOVEL     MSG(5)        oticmsg
036800060705     c                   movel     'D'           oticerr
036900060704     C                   GOTO      ENDCTR
037000060704    2C                   ENDIF
037100060704     c
037200060704     c                   movel     iticdiv       oticdiv
037300060704    1C                   ENDIF
037400060704     c
037500060704     C                   MOVEL     TBLUNI        DSCV
037600060704     C*
037700060704     C* VEDO SE AMMESSI IMPORTI DECIMALI NELL'IMPORTO
037800060704    1C     §CVFDC        IFNE      'S'
037900060704     C                   MOVE      iticcas       W0033             3 3
038000060704    2C     W0033         IFNE      0
038100060704     C                   MOVEL     MSG(6)        oticmsg
038200060705     c                   movel     'D'           oticerr
038300060704     C                   GOTO      ENDCTR
038400060704    2C                   ENDIF
038500060704    1C                   ENDIF
038600060704      *
038700060704      * Controllo il numero di decimali immessi
038800060704    1C     §CVFDC        IFEQ      'S'
038900060704     C     iticcas       ANDgt     *ZEROS
039000060704      *
039100060704    2C                   SELECT
039200060704     C     §CVDEC        WHENEQ    3
039300060704     C     §CVDEC        WHENEQ    2
039400060704     C                   MOVE      iticcas       W0010             1 0
039500060704    3C     W0010         IFNE      *ZEROS
039600060704     C                   MOVEA     MSG(7)        oticmsg
039700060704     c                   eval      %subst (oticmsg:29:2) = (%editc(§cvdec:'X'))
039800060705     c                   movel     'D'           oticerr
039900060704    3C                   ENDIF
040000060704    c
040100060704     C     §CVDEC        WHENEQ    1
040200060704     C                   MOVE      iticcas       W0020             2 0
040300060704    3C     W0020         IFNE      *ZEROS
040400060704     C                   MOVEA     MSG(7)        oticmsg
040500060704     c                   eval      %subst (oticmsg:29:2) = (%editc(§cvdec:'X'))
040600060705     c                   movel     'D'           oticerr
040700060704    3C                   ENDIF
040800060704    c
040900060704     C     §CVDEC        WHENEQ    0
041000060704    2C                   ENDSL
041100060704     c
041200060705    2c                   if        oticerr<>' '
041300060704     c                   goto      endctr
041400060704    2c                   endif
041500060704      *
041600060704    1C                   ENDIF
041700060704     C*
041800060704     C* SE DIVISA ESTERA VEDO SE POSSO ACCETTARE un TIPO INCASSO
041900060704     c*    non previsto per l'estero
042000060704    1C     §CVDVA        IFNE      'S'
042100060704     C     §1AUDE        ANDNE     'S'
042200060704     C                   MOVEL     MSG(8)        oticmsg
042300060705     c                   movel     'T'           oticerr
042400060704     C                   GOTO      ENDCTR
042500060704    2C                   END
042600060704      *
042700060704      * reperisco dati tabella 'CW'
042800060704     C                   CLEAR                   DSCW
042900060704     C                   MOVEL     'CW'          COD
043000060704     C                   MOVEL(P)  oticdiv       KEY
043100060704     C                   EXSR      CTRTAB
043200060704     C     *IN32         IFEQ      *OFF
043300060704     C                   MOVEL     TBLUNI        DSCW
043400060704     C                   ENDIF
043500060704     c
043600060704      * Controllo se la divisa immessa è valida per il tipo incasso
043700060704     C     §1ACDL        IFEQ      'S'
043800060704     C     iticdsp       ANDGT     §CWDLC
043900060704     C                   MOVEL     MSG(9)        oticmsg
044000060705     c                   movel     'D'           oticerr
044100060704     C                   GOTO      ENDCTR
044200060704     C                   ENDIF
044300060704     C*
044400060704     C* PER EXPORT BARTOLINI, DPD o FEDEX NON POSSO MODIFICARE LA DIVISA
044500060704     c*  al massimo l'eur
044600060704     C     oticdiv       IFNE      DESVLT
044700060704     C     oticdiv       ANDNE     'EUR'
044800060704     c     iticlnantw    ifeq      'EEX'
044900060704     c     iticlnantw    oreq      'DPD'
045000060704     c     iticlnantw    oreq      'FED'
045100060704     C                   MOVEL     MSG(10)       oticmsg
045200060705     c                   movel     'D'           oticerr
045300060704     C                   GOTO      ENDCTR
045400060704    2C                   ENDIF
045500060704     c                   ENDIF
045600011106
045700060704     c     ENDCTR        ENDSR
045800060704     c*
045900060704     C* RICERCA E CONTROLLO RECORD IN TABELLA ------------------------*
046000060704     C     CTRTAB        BEGSR
046100060704     C*  RICERCA
046200060704     C     oticric       IFne      ' '
046300060704     C                   MOVEL     COD           §COD
046400060704     C                   CLEAR                   §KEY
046500060704     C                   CALL      'X§TABER'
046600060704     C                   PARM                    §KUT
046700060704     C                   PARM                    §COD
046800060704     C                   PARM                    §KEY
046900060704     C                   PARM                    §DES             30
047000060704     C                   SETON                                        90
047100060704     C*
047200060704     C                   ELSE
047300060704     C*
047400060704     c* controllo
047500060704     C     KTAB          CHAIN     TABEL                              32
047600060704     C  N32TBLFLG        IFNE      ' '
047700060704     C                   SETON                                        32
047800060704     C                   END
047900060704     C                   END
048000060704     c
048100060704     C                   ENDSR
048200060704**         MSG
048300060704Tipo incasso C/Assegno errato                                                 01
048400060704Cliente non autorizzato al tipo incasso C/A indicato                          02
048500060704Tipo incasso non modificabile per questo cliente                              03
048600060704Divisa C/Assegno obbligatoria se presente importo                             04
048700060704Valuta C/Assegno errata                                                       05
048800060704Non ammessi importi con decimali per la valuta C/Assegno indicata             06
048900060704Ammessi importi con massimo __ decimali per la divisa indicata                07
049000060704Tipo incasso non utilizzabile se C/Assegno in divisa estera                   08
049100060704Tipo incasso non utilizzabile per la divisa del Contrassegno                  09
049200060704Bolla EXPORT: la divisa del C/A deve essere o quella del destinatario o "EUR" 10
049300060705Tipo Incasso C/Assegno inesistente o non e' da indicare in partenza           11
049400090706Tipo incasso non modificabile per questo destinatario                         12
049500100208Tipo incasso "SC" non ammesso per questa spedizione                           13
049600121023Spedizione export: Tipo incasso non abilitato per la linea di arrivo          14
