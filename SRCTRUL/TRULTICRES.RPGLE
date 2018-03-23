000100011029     H DECEDIT('0,') DATEDIT(*DMY.)
000200060704     H* TRULTICR *---------------------------------------------------*
000300060704     H*   - CONTROLLO TUPO INCASSO E DIVISA C/A
000400060704     H*--------------------------------------------------------------*
000500060704     ftabel00f  if   e           k disk
000600060704     fazorg01l  if   e           k disk
000700160510     D MSG             S             78    DIM(16) CTDATA PERRCD(1)
000800011019
000900060704     d ds15          e ds
001000060704     d ds1a          e ds
001100060704     d dsTM          e ds
001200060704     d dsCW          e ds
001300060704     d dsCV          e ds
001400160510     d dlca          e ds
001500071130     d kpjba         e ds
001600060704     d trulTICDS     e ds
001700011029     d tibs02DS      e ds
001800090706     D tisit0ds      E DS
001900090706     D tisit0x1ds    E DS
002000060704     d og143         e ds
002100011030
002200060704     d §kut                                like(tblkut) inz(1)
002300060704     d §cod                                like(tblcod)
002400060704     d COD                                 like(tblcod)
002500060704     d §key                                like(tblkey)
002600060704     d KEY                                 like(tblkey)
002700011019
002800011019      *----------------------------------------------------------------
002900060704     C     KTAB          KLIST
003000060704     C                   KFLD                    §kut
003100060704     C                   KFLD                    COD
003200060704     C                   KFLD                    KEY
003300060704      *----------------------------------------------------------------
003400011019
003500011029     c     *entry        plist
003600060704     c                   parm                    trulTICDS
003700060704     c                   parm                    DS1A
003800011106
003900011019
004000060704     c                   clear                   oTICRIC
004100060704     c                   clear                   oTICTIC
004200060704     c                   clear                   oTICDIV
004300060704     c                   clear                   oTICERR
004400060704     c                   clear                   oTICMSG
004500060704     c                   clear                   oTICRIC
004600060704     C* IMPOSTAZIONI
004700060704     C                   EXSR      IMPO
004800011019
004900011019      * Se Tipo lancio 'C' --> solo chiusura file
005000060704     c                   if        iTICTla <> 'C'
005100060704     c                   exsr      controlla
005200011029     c                   endif
005300011011
005400060704     c                   if        itictla <> *blanks
005500011031     c                   eval      *inlr = *on
005600090706      *
005700090706     c                   clear                   tisit0ds
005800090706     c                   clear                   tisit0x1ds
005900090706     c                   movel     'C'           it0tla
006000090706     C                   CALL      'TISIT5R'
006100090706     C                   PARM                    tisit0ds
006200090706     C                   PARM                    tisit0x1ds
006300090706      *
006400011029     c                   else
006500011031     c                   eval      *inrt = *on
006600011029     c                   endif
006700011011
006800060704      *----------------------------------------------------
006900060704      * IMPOSAZIONI INIZIALI
007000060704      *----------------------------------------------------
007100060704     C     IMPO          BEGSR
007200060704      * Controllo validità tipo lancio
007300060704     c                   if        iTICtla <> 'C' and iTICtla <> 'L' and
007400060704     c                             iTICtla <> *blanks
007500060704     c                   clear                   iTICTLA
007600060704     c                   endif
007700060704     C
007800060704     C                   IF        ITICTLA<>'C'
007900060704     c
008000060704     C                   IF        ITICLNANTW=*Blanks
008100060704     c     iticlna       chain     azorg01l
008200060704     c                   if        %found
008300060704     C                   MOVEL     ORGDE3        OG143
008400060704     C                   MOVEL     §OGNTW        iticlnantw
008500060704     c                   endif
008600060704     c                   endif
008700060704     c
008800060704     C* PRENDO LA VALUTA DAL DESTINATARIO
008900060704     C                   MOVEL     '15'          COD
009000060704     C                   MOVEL(P)  iticnzd       KEY
009100060704     C                   EXSR      CTRTAB
009200060704     C  N32              MOVEL     TBLUNI        DS15
009300060704     C   32              CLEAR                   DS15
009400060704     C                   MOVEL     §15VLT        DESVLT            3
009500060704     c
009600060704     c                   endif
009700060704     c
009800060704     c                   ENDSR
009900011011      *----------------------------------------------------
010000060704      * controlli
010100011011      *----------------------------------------------------
010200060704     c     controlla     begsr
010300060704     C***
010400060704     c*** T I P O     I N C A S S O
010500060704     C***
010600060704     C                   MOVEL     '1A'          COD
010700060704     C* RICERCA
010800060704     C     '?'           SCAN      itictic                                90
010900060704     C*
011000060704    1C     *IN90         IFEQ      *ON
011100060704     C                   MOVEL     'T'           oticric
011200060704     C                   ELSE
011300060704     C                   MOVEL(P)  itictic       KEY
011400060704    1C                   ENDIF
011500060704     C*
011600060704     C                   EXSR      CTRTAB
011700060704     C*
011800060704    1C     oticric       IFEQ      'T'
011900060704     C                   MOVEL     §KEY          otictic
012000060705     C                   GOTO      ENDCTR
012100060704   X1C                   ELSE
012200060704     C*
012300060704    2C     *IN32         IFEQ      *ON
012400060704     C                   MOVEL     MSG(1)        oticmsg
012500060705     c                   movel     'T'           oticerr
012600060704     C                   GOTO      ENDCTR
012700060704    2C                   ENDIF
012800060704     c*
012900060704     c                   movel     itictic       otictic
013000060704    1C                   ENDIF
013100060704     C*
013200060704     C                   MOVEL     TBLUNI        DS1A
013300060705     c
013400060705     c* Se bolla partenza deve essere utilizzabile in partenza
013500090625    1c                   if        itictbo='P' and §1afg1<>'1'
013600060705     C                   MOVEL     MSG(11)       oticmsg
013700060705     c                   movel     'T'           oticerr
013800060705     C                   GOTO      ENDCTR
013900090625    1C                   ENDif
014000060704     C*
014100060704     C*  Aggancio comunque la tabella TM per verificare se abilitazioni
014200060704     c*  oppure forzature
014300060704     C                   MOVEL     'TM'          COD
014400060704     C                   MOVEL(P)  iticccm       KEY
014500060704     C*
014600060704     C                   EXSR      CTRTAB
014700060704     C  n32              MOVEL     TBLUNI        DSTM
014800060704     c   32              clear                   dstm
014900060704     C*
015000090706     c* richiamo pgm per reperimento eventuale forzatura tipo
015100090706     c* incasso per destinatario particolare
015200090706     c                   clear                   tisit0ds
015300090706     c                   clear                   tisit0x1ds
015400090706     c                   movel     'E'           it0tla
015500090706     c                   movel     iticnzd       it0naz
015600090706     c                   movel     iticprd       it0prv
015700090706     c                   movel     iticcad       it0cap
015800090706     c                   movel     iticlod       it0loc
015900090706     c                   movel     iticind       it0ind
016000090706     c                   movel     iticrsd       it0rag
016100090706     c                   call      'TISIT5R'
016200090706     c                   parm                    tisit0ds
016300090706     c                   parm                    tisit0x1ds
016400060704     C* SE devo controllare la Tabella TM dal tipo incasso,
016500060704     c*  vedo se cliente autorizzato
016600060704     c
016700090625     c* Non effettuo il controllo se richiamo da FNLS01R - imm.bolle
016800090625     c*  con ultima posizione del campo PGM = '1'
016900090625     c*  (si tratta dell'indicatore 44 ovvero del richiamo dalle
017000090625     c*   giacenze che non deve effettuare questi controlli per
017100090625     c*   creazione di bolla legata)
017200120320     c* 20/03/2012 : Se richiamato da fnls01r controllo sempre su tabella "TM"
017300120320     c*              se filiale
017400120326   1ac***                if        %subst(iticpgm:1:7)<> 'FNLS01R' or
017500120326     c***                          %subst(iticpgm:10:1)<>'1' or
017600120326     c***                          (%subst(iticpgm:1:7)='FNLS01R' and iticlnp=5)
017700090625    1c                   if        §1ATTM='S'
017800060704     c*
017900060704    2C     *IN32         IFEQ      *ON
018000071130     C                   CLEAR                   tibs02ds
018100071130     c
018200071130    3c                   if        otictic ='SC'
018300071130     c                   movel     'L'           T02tla
018400071130     C                   MOVEL     'C'           T02MOD
018500071130     C                   MOVEL     'FSC'         T02COD
018600071130     c                   movel(P)  iticlna       t02ke1
018700071130     C                   CALL      'TIBS02R'
018800071130     C                   PARM                    KPJBA
018900071130     C                   PARM                    tibs02ds
019000071130    3c                   endif
019100090706    3C                   IF        ((T02ERR <>*BLANKS or t02atb<>*blanks) and
019200120801     c                               (OT0X1TICF=*blanks or OT0X1TICF<>otictic
019300120801     c                                or OT0ERR<>' '))
019400090706     c                             or (otictic<>'SC' and
019500120801     c                              (OT0X1TICF=*blanks or OT0X1TICF<>otictic
019600120801     c                                or OT0ERR<>' '))
019700100208     c* Tipo incasso "SC" inseribile in arrivo sempre solo se in bolla non c'e' alcun tipo incasso C/ASS
019800100208    4c                   if        otictic<>'SC' or itictbo='P'
019900111028    4c                                           or (iticticbol<>'  '
020000160708     c                                           and iticticbol<>'SC' and
020100160708     c                                           %subst(iticpgm:1:7)<>'FNLRG2R')
020200100208    4c                   if        otictic<>'SC'
020300060704     C                   MOVEL     MSG(2)        oticmsg
020400100208     c                   else
020500100208     C                   MOVEL     MSG(13)       oticmsg
020600100208     c                   endif
020700091211     c                   movel     'C'           oticerr
020800060704     C                   GOTO      ENDCTR
020900100208    4C                   ENDIF
021000100208    3C                   ENDIF
021100100208     c
021200071130   x2c                   else
021300060704     C*
021400060704     C* TIC = 'TM'
021500060704     C                   SELECT
021600071130    3C     otictic       WHENEQ    'TM'
021700071130    4C     §TMFTM        IFNE      'S'
021800060704     C                   SETON                                        32
021900071130    4C                   END
022000060704     C* TIC = 'BB'
022100071130    3C     otictic       WHENEQ    'BB'
022200060704     C     otictic       OREQ      'CB'
022300071130    4C     §TMFBB        IFNE      'S'
022400060704     C     §TMFBC        OREQ      'S'
022500060704     C                   SETON                                        32
022600071130    4C                   END
022700060704     C* TIC = 'BC'
022800071130    3C     otictic       WHENEQ    'BC'
022900071130    4C     §TMFBC        IFNE      'S'
023000060704     C                   SETON                                        32
023100071130    4C                   END
023200060704     C* TIC = 'BA'
023300071130    3C     otictic       WHENEQ    'BA'
023400071130    4C     §TMFBA        IFNE      'S'
023500060704     C                   SETON                                        32
023600071130    4C                   END
023700060704     C* TIC = 'CA'
023800071130    3C     otictic       WHENEQ    'CA'
023900071130    4C     §TMFCA        IFNE      'S'
024000060704     C                   SETON                                        32
024100071130    4C                   END
024200090617     C* TIC = 'BP'
024300090617    3C     otictic       WHENEQ    'BP'
024400090617    4C     §TMFBP        IFNE      'S'
024500090617     C                   SETON                                        32
024600090617    4C                   END
024700150520     C* TIC = 'TP'
024800150520    3C     otictic       WHENEQ    'TP'
024900150520    4C     §TMFTP        IFNE      'S'
025000150520     C                   SETON                                        32
025100150520    4C                   END
025200060704     C* TIC = 'SC'
025300071130    3C     otictic       WHENEQ    'SC'
025400071130    4C     §TMFSC        IFNE      'S'
025500071130     c* Verifico anche tabella FSC  forzatura per lna
025600071130     C                   CLEAR                   tibs02ds
025700071130     c                   movel     'L'           T02tla
025800071130     C                   MOVEL     'C'           T02MOD
025900071130     C                   MOVEL     'FSC'         T02COD
026000071130     c                   movel(P)  iticlna       t02ke1
026100071130     C                   CALL      'TIBS02R'
026200071130     C                   PARM                    KPJBA
026300071130     C                   PARM                    tibs02ds
026400100205     c
026500071130    5C                   IF        T02ERR <>*BLANKS or  t02atb<>*blanks
026600100205     c
026700100205     c* Tipo incasso "SC" inseribile in arrivo sempre solo se in bolla non c'e' alcun tipo incasso C/ASS
026800160708    1c                   if        itictbo='P' or (iticticbol<>'  ' and
026900160720     c                                           iticticbol<>'SC' and
027000160708     c                                           %subst(iticpgm:1:7)<>'FNLRG2R')
027100060704     C                   SETON                                        32
027200100205    5C                   ENDif
027300100205    5C                   END
027400071130    4C                   END
027500060704     C* TIC = 'TO'
027600071130    3C     otictic       WHENEQ    'TO'
027700071130    4C     §TMFTO        IFNE      'S'
027800060704     C                   SETON                                        32
027900071130    4C                   END
028000080625     C* TIC = 'S2'
028100080625    3C     otictic       WHENEQ    'S2'
028200080625    4C     §TMFS2        IFNE      'S'
028300080625     C                   SETON                                        32
028400080625    4C                   END
028500071130    3C                   ENDSL
028600060704     C*
028700090706    4C***  *IN32         IFEQ      *ON
028800090706     c                   if        *in32 and
028900120802     c                             (OT0X1TICF=*blanks or OT0X1TICF<>otictic
029000120802     c                             or OT0ERR<>' ')
029100060704     C                   MOVEL     MSG(2)        oticmsg
029200091211     c                   movel     'C'           oticerr
029300060704     C                   GOTO      ENDCTR
029400071130    4C                   END
029500071130    2C                   END
029600060704     c
029700060704    1c                   endif
029800120326   1ac******             endif
029900121022     c*
030000121022     c* Se spedizione export controllo che il tipo incasso sia gestito dal partner
030100121022     c                   if        (iticlnantw='DPD' or iticlnantw='FED' or
030200121022     c                                 iticlnantw='EEX')
030300121022     c                             and %subst(iticpgm:1:7)='FNLS01R'
030400121022     C                   CLEAR                   tibs02ds
030500121022     c                   movel     'L'           T02tla
030600121022     C                   MOVEL     'C'           T02MOD
030700121022     C                   MOVEL     '1AX'         T02COD
030800121022     c                   movel(P)  iticlna       t02ke1
030900121022     c                   movel(P)  otictic       t02ke2
031000121022     C                   CALL      'TIBS02R'
031100121022     C                   PARM                    KPJBA
031200121022     C                   PARM                    tibs02ds
031300121022     c                   if        t02err<>*blanks or t02atb<>*blanks
031400121022     C                   MOVEL     MSG(14)       oticmsg
031500121023     c                   eval      oticmsg=%trim(oticmsg)+' ' +
031600121023     c                                     %editc(iticlna:'X')
031700121022     c                   movel     'C'           oticerr
031800121022     c                   endif
031900121022     c                   endif
032000060704     c
032100120802     c* Verifico se ha forzature in manutenzione: se tipo incasso forzabile
032200060704     c*  in partenza, non modificabile
032300160908     c*  se il tipo incasso è quello forzato. E' capitato infatto che un cliente
032400160908     c*  avesse in bolla CM e non potevano mettere SC perchè lui aveva la frozatura, ma per SC non per CM
032500060704    1c                   if        §tmfti<>'  ' and otictic<>iticTICbol and
032600160908     c                             iticticbol<>*blanks   and iticticbol=§tmfti
032700121022     c                             and iticlnantw<>'DPD'
032800121022     c                             and iticlnantw<>'EEX'
032900121022     c                             and iticlnantw<>'FED'
033000170622     c* Se supera limite per contate, allora non do errore
033100170622     c
033200060704     C                   MOVEL     MSG(3)        oticmsg
033300091211     c                   movel     'C'           oticerr
033400060704     C                   GOTO      ENDCTR
033500060704    1C                   ENDif
033600170622    c
033700120802     c                   if        ot0err=' ' and
033800120802    1c                             OT0X1TICF<>*blanks and itictbo='A' and
033900090706     c                             otictic<>iticticbol and
034000090706     c                             iticticbol<>*blanks
034100121022     c                             and iticlnantw<>'DPD'
034200121022     c                             and iticlnantw<>'EEX'
034300121022     c                             and iticlnantw<>'FED'
034400090706     C                   MOVEL     MSG(12)       oticmsg
034500090706     c                   movel     'T'           oticerr
034600090706     C                   GOTO      ENDCTR
034700090706    1C                   ENDif
034800160510     c
034900160510     c                   if        %subst(iticpgm:1:7)=  'FNLRG2R'
035000160510     c                             and  itictic=*blanks
035100160510     C                   MOVEL     MSG(16)       oticmsg
035200160510     c                   movel     'T'           oticerr
035300160510     C                   GOTO      ENDCTR
035400160510     c                   endif
035500170622     c
035600160510     c* se tipo incasso "SOLO CONTANTE" non è accettabile un importo > del limite previsto
035700160510     c*  di legge. Tale limite lo prendo dal record 017 solo per contatti DEST
035800170622     c                   exsr      Ctr_cont
035900160510     c
036000060704     C***
036100060704     C***   D I V I S A
036200060704     C***
036300060704      * La divisa è obbligatoria se è inserito un importo
036400060704    1C     ITICCAS       IFGT      *ZEROS
036500060704     C     ITICdiv       ANDEQ     *BLANKS
036600060704     C                   MOVEL     MSG(4)        OTICMSG
036700060705     c                   movel     'D'           oticerr
036800060704     C                   GOTO      ENDCTR
036900060704    1C                   ENDIF
037000060704     C*
037100060704     C                   MOVEL     'CV'          COD
037200060704     C* RICERCA
037300060704     C     '?'           SCAN      iticdiv                                90
037400060704    1C     *IN90         IFEQ      *ON
037500060704     C                   MOVEL     'D'           oticric
037600060704   x1C                   ELSE
037700060704     C                   MOVEL(P)  iticdiv       KEY
037800060704    1C                   ENDIF
037900060704     C*
038000060704     C                   EXSR      CTRTAB
038100060704     C*
038200060704    1C     oticric       IFne      ' '
038300060704     C                   MOVEL     §KEY          oticdiv
038400060705     C                   GOTO      ENDCTR
038500060704   X1C                   ELSE
038600060704     C* ERRORE
038700060704    2C     *IN32         IFEQ      *ON
038800060704     C                   MOVEL     MSG(5)        oticmsg
038900060705     c                   movel     'D'           oticerr
039000060704     C                   GOTO      ENDCTR
039100060704    2C                   ENDIF
039200060704     c
039300060704     c                   movel     iticdiv       oticdiv
039400060704    1C                   ENDIF
039500060704     c
039600060704     C                   MOVEL     TBLUNI        DSCV
039700060704     C*
039800060704     C* VEDO SE AMMESSI IMPORTI DECIMALI NELL'IMPORTO
039900060704    1C     §CVFDC        IFNE      'S'
040000060704     C                   MOVE      iticcas       W0033             3 3
040100060704    2C     W0033         IFNE      0
040200060704     C                   MOVEL     MSG(6)        oticmsg
040300060705     c                   movel     'D'           oticerr
040400060704     C                   GOTO      ENDCTR
040500060704    2C                   ENDIF
040600060704    1C                   ENDIF
040700060704      *
040800060704      * Controllo il numero di decimali immessi
040900060704    1C     §CVFDC        IFEQ      'S'
041000060704     C     iticcas       ANDgt     *ZEROS
041100060704      *
041200060704    2C                   SELECT
041300060704     C     §CVDEC        WHENEQ    3
041400060704     C     §CVDEC        WHENEQ    2
041500060704     C                   MOVE      iticcas       W0010             1 0
041600060704    3C     W0010         IFNE      *ZEROS
041700060704     C                   MOVEA     MSG(7)        oticmsg
041800060704     c                   eval      %subst (oticmsg:29:2) = (%editc(§cvdec:'X'))
041900060705     c                   movel     'D'           oticerr
042000060704    3C                   ENDIF
042100060704    c
042200060704     C     §CVDEC        WHENEQ    1
042300060704     C                   MOVE      iticcas       W0020             2 0
042400060704    3C     W0020         IFNE      *ZEROS
042500060704     C                   MOVEA     MSG(7)        oticmsg
042600060704     c                   eval      %subst (oticmsg:29:2) = (%editc(§cvdec:'X'))
042700060705     c                   movel     'D'           oticerr
042800060704    3C                   ENDIF
042900060704    c
043000060704     C     §CVDEC        WHENEQ    0
043100060704    2C                   ENDSL
043200060704     c
043300060705    2c                   if        oticerr<>' '
043400060704     c                   goto      endctr
043500060704    2c                   endif
043600060704      *
043700060704    1C                   ENDIF
043800060704     C*
043900060704     C* SE DIVISA ESTERA VEDO SE POSSO ACCETTARE un TIPO INCASSO
044000060704     c*    non previsto per l'estero
044100060704    1C     §CVDVA        IFNE      'S'
044200060704     C     §1AUDE        ANDNE     'S'
044300060704     C                   MOVEL     MSG(8)        oticmsg
044400060705     c                   movel     'T'           oticerr
044500060704     C                   GOTO      ENDCTR
044600060704    2C                   END
044700060704      *
044800060704      * reperisco dati tabella 'CW'
044900060704     C                   CLEAR                   DSCW
045000060704     C                   MOVEL     'CW'          COD
045100060704     C                   MOVEL(P)  oticdiv       KEY
045200060704     C                   EXSR      CTRTAB
045300060704     C     *IN32         IFEQ      *OFF
045400060704     C                   MOVEL     TBLUNI        DSCW
045500060704     C                   ENDIF
045600060704     c
045700060704      * Controllo se la divisa immessa è valida per il tipo incasso
045800060704     C     §1ACDL        IFEQ      'S'
045900060704     C     iticdsp       ANDGT     §CWDLC
046000060704     C                   MOVEL     MSG(9)        oticmsg
046100060705     c                   movel     'D'           oticerr
046200060704     C                   GOTO      ENDCTR
046300060704     C                   ENDIF
046400060704     C*
046500060704     C* PER EXPORT BARTOLINI, DPD o FEDEX NON POSSO MODIFICARE LA DIVISA
046600060704     c*  al massimo l'eur
046700060704     C     oticdiv       IFNE      DESVLT
046800060704     C     oticdiv       ANDNE     'EUR'
046900060704     c     iticlnantw    ifeq      'EEX'
047000060704     c     iticlnantw    oreq      'DPD'
047100060704     c     iticlnantw    oreq      'FED'
047200060704     C                   MOVEL     MSG(10)       oticmsg
047300060705     c                   movel     'D'           oticerr
047400060704     C                   GOTO      ENDCTR
047500060704    2C                   ENDIF
047600060704     c                   ENDIF
047700011106
047800060704     c     ENDCTR        ENDSR
047900170622     c
048000170622     c* controllo massimo importo per contante----------------------------------*
048100170622     c     CTR_cont      BEGSR
048200170622     c                   if        %subst(iticpgm:1:7)=  'FNLRG2R'
048300170622     c                             and §1afca='S'
048400170622     c                   clear                   dlca
048500170622     c                   clear                   tibs02ds
048600170622     c                   movel     'C'           t02mod
048700170622     c                   movel     knsif         t02sif
048800170622     c                   movel     'LCA'         t02cod
048900170622     c                   movel(p)  '017'         t02ke1
049000170622     C                   CALL      'TIBS02R'
049100170622     C                   PARM                    KPJBA
049200170622     C                   PARM                    tibs02ds
049300170622     c                   if        t02err= *blanks and t02atb= *blanks
049400170622     c                   eval      dlca=t02uni
049500170622     c                   if        iticcas>§LCACO
049600170622     C                   MOVEL     MSG(15)       oticmsg
049700170622     c                   movel     'T'           oticerr
049800170622     C                   GOTO      ENDCTR
049900170622     c                   endif
050000170622     c                   endif
050100170622     c                   endif
050200170622     c                   ENDSR
050300060704     c*
050400060704     C* RICERCA E CONTROLLO RECORD IN TABELLA ------------------------*
050500060704     C     CTRTAB        BEGSR
050600060704     C*  RICERCA
050700060704     C     oticric       IFne      ' '
050800060704     C                   MOVEL     COD           §COD
050900060704     C                   CLEAR                   §KEY
051000060704     C                   CALL      'X§TABER'
051100060704     C                   PARM                    §KUT
051200060704     C                   PARM                    §COD
051300060704     C                   PARM                    §KEY
051400060704     C                   PARM                    §DES             30
051500060704     C                   SETON                                        90
051600060704     C*
051700060704     C                   ELSE
051800060704     C*
051900060704     c* controllo
052000060704     C     KTAB          CHAIN     TABEL                              32
052100060704     C  N32TBLFLG        IFNE      ' '
052200060704     C                   SETON                                        32
052300060704     C                   END
052400060704     C                   END
052500060704     c
052600060704     C                   ENDSR
052700060704**         MSG
052800060704Tipo incasso C/Assegno errato                                                 01
052900060704Cliente non autorizzato al tipo incasso C/A indicato                          02
053000060704Tipo incasso non modificabile per questo cliente                              03
053100060704Divisa C/Assegno obbligatoria se presente importo                             04
053200060704Valuta C/Assegno errata                                                       05
053300060704Non ammessi importi con decimali per la valuta C/Assegno indicata             06
053400060704Ammessi importi con massimo __ decimali per la divisa indicata                07
053500060704Tipo incasso non utilizzabile se C/Assegno in divisa estera                   08
053600060704Tipo incasso non utilizzabile per la divisa del Contrassegno                  09
053700060704Bolla EXPORT: la divisa del C/A deve essere o quella del destinatario o "EUR" 10
053800060705Tipo Incasso C/Assegno inesistente o non e' da indicare in partenza           11
053900090706Tipo incasso non modificabile per questo destinatario                         12
054000100208Tipo incasso "SC" non ammesso per questa spedizione                           13
054100121023Spedizione export: Tipo incasso non abilitato per la linea di arrivo          14
054200160510Importo C/Assegno supera il limite previsto di legge per i CONTANTI           15
054300160510Obbligatorio indicare il tipo incasso C/Assegno                               16
