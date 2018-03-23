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
020001160708     c                                           %subst(iticpgm:1:7)<>'FNLRG2R')
020100100208    4c                   if        otictic<>'SC'
020200060704     C                   MOVEL     MSG(2)        oticmsg
020300100208     c                   else
020400100208     C                   MOVEL     MSG(13)       oticmsg
020500100208     c                   endif
020600091211     c                   movel     'C'           oticerr
020700060704     C                   GOTO      ENDCTR
020800100208    4C                   ENDIF
020900100208    3C                   ENDIF
021000100208     c
021100071130   x2c                   else
021200060704     C*
021300060704     C* TIC = 'TM'
021400060704     C                   SELECT
021500071130    3C     otictic       WHENEQ    'TM'
021600071130    4C     §TMFTM        IFNE      'S'
021700060704     C                   SETON                                        32
021800071130    4C                   END
021900060704     C* TIC = 'BB'
022000071130    3C     otictic       WHENEQ    'BB'
022100060704     C     otictic       OREQ      'CB'
022200071130    4C     §TMFBB        IFNE      'S'
022300060704     C     §TMFBC        OREQ      'S'
022400060704     C                   SETON                                        32
022500071130    4C                   END
022600060704     C* TIC = 'BC'
022700071130    3C     otictic       WHENEQ    'BC'
022800071130    4C     §TMFBC        IFNE      'S'
022900060704     C                   SETON                                        32
023000071130    4C                   END
023100060704     C* TIC = 'BA'
023200071130    3C     otictic       WHENEQ    'BA'
023300071130    4C     §TMFBA        IFNE      'S'
023400060704     C                   SETON                                        32
023500071130    4C                   END
023600060704     C* TIC = 'CA'
023700071130    3C     otictic       WHENEQ    'CA'
023800071130    4C     §TMFCA        IFNE      'S'
023900060704     C                   SETON                                        32
024000071130    4C                   END
024100090617     C* TIC = 'BP'
024200090617    3C     otictic       WHENEQ    'BP'
024300090617    4C     §TMFBP        IFNE      'S'
024400090617     C                   SETON                                        32
024500090617    4C                   END
024600150520     C* TIC = 'TP'
024700150520    3C     otictic       WHENEQ    'TP'
024800150520    4C     §TMFTP        IFNE      'S'
024900150520     C                   SETON                                        32
025000150520    4C                   END
025100060704     C* TIC = 'SC'
025200071130    3C     otictic       WHENEQ    'SC'
025300071130    4C     §TMFSC        IFNE      'S'
025400071130     c* Verifico anche tabella FSC  forzatura per lna
025500071130     C                   CLEAR                   tibs02ds
025600071130     c                   movel     'L'           T02tla
025700071130     C                   MOVEL     'C'           T02MOD
025800071130     C                   MOVEL     'FSC'         T02COD
025900071130     c                   movel(P)  iticlna       t02ke1
026000071130     C                   CALL      'TIBS02R'
026100071130     C                   PARM                    KPJBA
026200071130     C                   PARM                    tibs02ds
026300100205     c
026400071130    5C                   IF        T02ERR <>*BLANKS or  t02atb<>*blanks
026500100205     c
026600100205     c* Tipo incasso "SC" inseribile in arrivo sempre solo se in bolla non c'e' alcun tipo incasso C/ASS
026700160708    1c                   if        itictbo='P' or (iticticbol<>'  ' and
026701160720     c                                           iticticbol<>'SC' and
026702160708     c                                           %subst(iticpgm:1:7)<>'FNLRG2R')
026800060704     C                   SETON                                        32
026900100205    5C                   ENDif
027000100205    5C                   END
027100071130    4C                   END
027200060704     C* TIC = 'TO'
027300071130    3C     otictic       WHENEQ    'TO'
027400071130    4C     §TMFTO        IFNE      'S'
027500060704     C                   SETON                                        32
027600071130    4C                   END
027700080625     C* TIC = 'S2'
027800080625    3C     otictic       WHENEQ    'S2'
027900080625    4C     §TMFS2        IFNE      'S'
028000080625     C                   SETON                                        32
028100080625    4C                   END
028200071130    3C                   ENDSL
028300060704     C*
028400090706    4C***  *IN32         IFEQ      *ON
028500090706     c                   if        *in32 and
028600120802     c                             (OT0X1TICF=*blanks or OT0X1TICF<>otictic
028700120802     c                             or OT0ERR<>' ')
028800060704     C                   MOVEL     MSG(2)        oticmsg
028900091211     c                   movel     'C'           oticerr
029000060704     C                   GOTO      ENDCTR
029100071130    4C                   END
029200071130    2C                   END
029300060704     c
029400060704    1c                   endif
029500120326   1ac******             endif
029600121022     c*
029700121022     c* Se spedizione export controllo che il tipo incasso sia gestito dal partner
029800121022     c                   if        (iticlnantw='DPD' or iticlnantw='FED' or
029900121022     c                                 iticlnantw='EEX')
030000121022     c                             and %subst(iticpgm:1:7)='FNLS01R'
030100121022     C                   CLEAR                   tibs02ds
030200121022     c                   movel     'L'           T02tla
030300121022     C                   MOVEL     'C'           T02MOD
030400121022     C                   MOVEL     '1AX'         T02COD
030500121022     c                   movel(P)  iticlna       t02ke1
030600121022     c                   movel(P)  otictic       t02ke2
030700121022     C                   CALL      'TIBS02R'
030800121022     C                   PARM                    KPJBA
030900121022     C                   PARM                    tibs02ds
031000121022     c                   if        t02err<>*blanks or t02atb<>*blanks
031100121022     C                   MOVEL     MSG(14)       oticmsg
031200121023     c                   eval      oticmsg=%trim(oticmsg)+' ' +
031300121023     c                                     %editc(iticlna:'X')
031400121022     c                   movel     'C'           oticerr
031500121022     c                   endif
031600121022     c                   endif
031700060704     c
031800120802     c* Verifico se ha forzature in manutenzione: se tipo incasso forzabile
031900060704     c*  in partenza, non modificabile
032000060704    1c                   if        §tmfti<>'  ' and otictic<>iticTICbol and
032100060704     c                             iticticbol<>*blanks
032200121022     c                             and iticlnantw<>'DPD'
032300121022     c                             and iticlnantw<>'EEX'
032400121022     c                             and iticlnantw<>'FED'
032500060704     C                   MOVEL     MSG(3)        oticmsg
032600091211     c                   movel     'C'           oticerr
032700060704     C                   GOTO      ENDCTR
032800060704    1C                   ENDif
032900120802     c                   if        ot0err=' ' and
033000120802    1c                             OT0X1TICF<>*blanks and itictbo='A' and
033100090706     c                             otictic<>iticticbol and
033200090706     c                             iticticbol<>*blanks
033300121022     c                             and iticlnantw<>'DPD'
033400121022     c                             and iticlnantw<>'EEX'
033500121022     c                             and iticlnantw<>'FED'
033600090706     C                   MOVEL     MSG(12)       oticmsg
033700090706     c                   movel     'T'           oticerr
033800090706     C                   GOTO      ENDCTR
033900090706    1C                   ENDif
034000160510     c
034100160510     c                   if        %subst(iticpgm:1:7)=  'FNLRG2R'
034200160510     c                             and  itictic=*blanks
034300160510     C                   MOVEL     MSG(16)       oticmsg
034400160510     c                   movel     'T'           oticerr
034500160510     C                   GOTO      ENDCTR
034600160510     c                   endif
034700160510     c* se tipo incasso "SOLO CONTANTE" non è accettabile un importo > del limite previsto
034800160510     c*  di legge. Tale limite lo prendo dal record 017 solo per contatti DEST
034900160510     c
035000160510     c                   if        %subst(iticpgm:1:7)=  'FNLRG2R'
035100160510     c                             and §1afca='S'
035200160510     c                   clear                   dlca
035300160510     c                   clear                   tibs02ds
035400160510     c                   movel     'C'           t02mod
035500160510     c                   movel     knsif         t02sif
035600160510     c                   movel     'LCA'         t02cod
035700160510     c                   movel(p)  '017'         t02ke1
035800160510     C                   CALL      'TIBS02R'
035900160510     C                   PARM                    KPJBA
036000160510     C                   PARM                    tibs02ds
036100160510     c                   if        t02err= *blanks and t02atb= *blanks
036200160510     c                   eval      dlca=t02uni
036300160510     c                   if        iticcas>§LCACO
036400160510     C                   MOVEL     MSG(15)       oticmsg
036500160510     c                   movel     'T'           oticerr
036600160510     C                   GOTO      ENDCTR
036700160510     c                   endif
036800160510     c                   endif
036900160510     c                   endif
037000060704     C***
037100060704     C***   D I V I S A
037200060704     C***
037300060704      * La divisa è obbligatoria se è inserito un importo
037400060704    1C     ITICCAS       IFGT      *ZEROS
037500060704     C     ITICdiv       ANDEQ     *BLANKS
037600060704     C                   MOVEL     MSG(4)        OTICMSG
037700060705     c                   movel     'D'           oticerr
037800060704     C                   GOTO      ENDCTR
037900060704    1C                   ENDIF
038000060704     C*
038100060704     C                   MOVEL     'CV'          COD
038200060704     C* RICERCA
038300060704     C     '?'           SCAN      iticdiv                                90
038400060704    1C     *IN90         IFEQ      *ON
038500060704     C                   MOVEL     'D'           oticric
038600060704   x1C                   ELSE
038700060704     C                   MOVEL(P)  iticdiv       KEY
038800060704    1C                   ENDIF
038900060704     C*
039000060704     C                   EXSR      CTRTAB
039100060704     C*
039200060704    1C     oticric       IFne      ' '
039300060704     C                   MOVEL     §KEY          oticdiv
039400060705     C                   GOTO      ENDCTR
039500060704   X1C                   ELSE
039600060704     C* ERRORE
039700060704    2C     *IN32         IFEQ      *ON
039800060704     C                   MOVEL     MSG(5)        oticmsg
039900060705     c                   movel     'D'           oticerr
040000060704     C                   GOTO      ENDCTR
040100060704    2C                   ENDIF
040200060704     c
040300060704     c                   movel     iticdiv       oticdiv
040400060704    1C                   ENDIF
040500060704     c
040600060704     C                   MOVEL     TBLUNI        DSCV
040700060704     C*
040800060704     C* VEDO SE AMMESSI IMPORTI DECIMALI NELL'IMPORTO
040900060704    1C     §CVFDC        IFNE      'S'
041000060704     C                   MOVE      iticcas       W0033             3 3
041100060704    2C     W0033         IFNE      0
041200060704     C                   MOVEL     MSG(6)        oticmsg
041300060705     c                   movel     'D'           oticerr
041400060704     C                   GOTO      ENDCTR
041500060704    2C                   ENDIF
041600060704    1C                   ENDIF
041700060704      *
041800060704      * Controllo il numero di decimali immessi
041900060704    1C     §CVFDC        IFEQ      'S'
042000060704     C     iticcas       ANDgt     *ZEROS
042100060704      *
042200060704    2C                   SELECT
042300060704     C     §CVDEC        WHENEQ    3
042400060704     C     §CVDEC        WHENEQ    2
042500060704     C                   MOVE      iticcas       W0010             1 0
042600060704    3C     W0010         IFNE      *ZEROS
042700060704     C                   MOVEA     MSG(7)        oticmsg
042800060704     c                   eval      %subst (oticmsg:29:2) = (%editc(§cvdec:'X'))
042900060705     c                   movel     'D'           oticerr
043000060704    3C                   ENDIF
043100060704    c
043200060704     C     §CVDEC        WHENEQ    1
043300060704     C                   MOVE      iticcas       W0020             2 0
043400060704    3C     W0020         IFNE      *ZEROS
043500060704     C                   MOVEA     MSG(7)        oticmsg
043600060704     c                   eval      %subst (oticmsg:29:2) = (%editc(§cvdec:'X'))
043700060705     c                   movel     'D'           oticerr
043800060704    3C                   ENDIF
043900060704    c
044000060704     C     §CVDEC        WHENEQ    0
044100060704    2C                   ENDSL
044200060704     c
044300060705    2c                   if        oticerr<>' '
044400060704     c                   goto      endctr
044500060704    2c                   endif
044600060704      *
044700060704    1C                   ENDIF
044800060704     C*
044900060704     C* SE DIVISA ESTERA VEDO SE POSSO ACCETTARE un TIPO INCASSO
045000060704     c*    non previsto per l'estero
045100060704    1C     §CVDVA        IFNE      'S'
045200060704     C     §1AUDE        ANDNE     'S'
045300060704     C                   MOVEL     MSG(8)        oticmsg
045400060705     c                   movel     'T'           oticerr
045500060704     C                   GOTO      ENDCTR
045600060704    2C                   END
045700060704      *
045800060704      * reperisco dati tabella 'CW'
045900060704     C                   CLEAR                   DSCW
046000060704     C                   MOVEL     'CW'          COD
046100060704     C                   MOVEL(P)  oticdiv       KEY
046200060704     C                   EXSR      CTRTAB
046300060704     C     *IN32         IFEQ      *OFF
046400060704     C                   MOVEL     TBLUNI        DSCW
046500060704     C                   ENDIF
046600060704     c
046700060704      * Controllo se la divisa immessa è valida per il tipo incasso
046800060704     C     §1ACDL        IFEQ      'S'
046900060704     C     iticdsp       ANDGT     §CWDLC
047000060704     C                   MOVEL     MSG(9)        oticmsg
047100060705     c                   movel     'D'           oticerr
047200060704     C                   GOTO      ENDCTR
047300060704     C                   ENDIF
047400060704     C*
047500060704     C* PER EXPORT BARTOLINI, DPD o FEDEX NON POSSO MODIFICARE LA DIVISA
047600060704     c*  al massimo l'eur
047700060704     C     oticdiv       IFNE      DESVLT
047800060704     C     oticdiv       ANDNE     'EUR'
047900060704     c     iticlnantw    ifeq      'EEX'
048000060704     c     iticlnantw    oreq      'DPD'
048100060704     c     iticlnantw    oreq      'FED'
048200060704     C                   MOVEL     MSG(10)       oticmsg
048300060705     c                   movel     'D'           oticerr
048400060704     C                   GOTO      ENDCTR
048500060704    2C                   ENDIF
048600060704     c                   ENDIF
048700011106
048800060704     c     ENDCTR        ENDSR
048900060704     c*
049000060704     C* RICERCA E CONTROLLO RECORD IN TABELLA ------------------------*
049100060704     C     CTRTAB        BEGSR
049200060704     C*  RICERCA
049300060704     C     oticric       IFne      ' '
049400060704     C                   MOVEL     COD           §COD
049500060704     C                   CLEAR                   §KEY
049600060704     C                   CALL      'X§TABER'
049700060704     C                   PARM                    §KUT
049800060704     C                   PARM                    §COD
049900060704     C                   PARM                    §KEY
050000060704     C                   PARM                    §DES             30
050100060704     C                   SETON                                        90
050200060704     C*
050300060704     C                   ELSE
050400060704     C*
050500060704     c* controllo
050600060704     C     KTAB          CHAIN     TABEL                              32
050700060704     C  N32TBLFLG        IFNE      ' '
050800060704     C                   SETON                                        32
050900060704     C                   END
051000060704     C                   END
051100060704     c
051200060704     C                   ENDSR
051300060704**         MSG
051400060704Tipo incasso C/Assegno errato                                                 01
051500060704Cliente non autorizzato al tipo incasso C/A indicato                          02
051600060704Tipo incasso non modificabile per questo cliente                              03
051700060704Divisa C/Assegno obbligatoria se presente importo                             04
051800060704Valuta C/Assegno errata                                                       05
051900060704Non ammessi importi con decimali per la valuta C/Assegno indicata             06
052000060704Ammessi importi con massimo __ decimali per la divisa indicata                07
052100060704Tipo incasso non utilizzabile se C/Assegno in divisa estera                   08
052200060704Tipo incasso non utilizzabile per la divisa del Contrassegno                  09
052300060704Bolla EXPORT: la divisa del C/A deve essere o quella del destinatario o "EUR" 10
052400060705Tipo Incasso C/Assegno inesistente o non e' da indicare in partenza           11
052500090706Tipo incasso non modificabile per questo destinatario                         12
052600100208Tipo incasso "SC" non ammesso per questa spedizione                           13
052700121023Spedizione export: Tipo incasso non abilitato per la linea di arrivo          14
052800160510Importo C/Assegno supera il limite previsto di legge per i CONTANTI           15
052900160510Obbligatorio indicare il tipo incasso C/Assegno                               16
