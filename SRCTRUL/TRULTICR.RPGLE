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
002700170623     d werr            s              1
002800011019
002900011019      *----------------------------------------------------------------
003000060704     C     KTAB          KLIST
003100060704     C                   KFLD                    §kut
003200060704     C                   KFLD                    COD
003300060704     C                   KFLD                    KEY
003400060704      *----------------------------------------------------------------
003500011019
003600011029     c     *entry        plist
003700060704     c                   parm                    trulTICDS
003800060704     c                   parm                    DS1A
003900011106
004000011019
004100060704     c                   clear                   oTICRIC
004200060704     c                   clear                   oTICTIC
004300060704     c                   clear                   oTICDIV
004400060704     c                   clear                   oTICERR
004500060704     c                   clear                   oTICMSG
004600060704     c                   clear                   oTICRIC
004700060704     C* IMPOSTAZIONI
004800060704     C                   EXSR      IMPO
004900011019
005000011019      * Se Tipo lancio 'C' --> solo chiusura file
005100060704     c                   if        iTICTla <> 'C'
005200060704     c                   exsr      controlla
005300011029     c                   endif
005400011011
005500060704     c                   if        itictla <> *blanks
005600011031     c                   eval      *inlr = *on
005700090706      *
005800090706     c                   clear                   tisit0ds
005900090706     c                   clear                   tisit0x1ds
006000090706     c                   movel     'C'           it0tla
006100090706     C                   CALL      'TISIT5R'
006200090706     C                   PARM                    tisit0ds
006300090706     C                   PARM                    tisit0x1ds
006400090706      *
006500011029     c                   else
006600011031     c                   eval      *inrt = *on
006700011029     c                   endif
006800011011
006900060704      *----------------------------------------------------
007000060704      * IMPOSAZIONI INIZIALI
007100060704      *----------------------------------------------------
007200060704     C     IMPO          BEGSR
007300060704      * Controllo validità tipo lancio
007400060704     c                   if        iTICtla <> 'C' and iTICtla <> 'L' and
007500060704     c                             iTICtla <> *blanks
007600060704     c                   clear                   iTICTLA
007700060704     c                   endif
007800060704     C
007900060704     C                   IF        ITICTLA<>'C'
008000060704     c
008100060704     C                   IF        ITICLNANTW=*Blanks
008200060704     c     iticlna       chain     azorg01l
008300060704     c                   if        %found
008400060704     C                   MOVEL     ORGDE3        OG143
008500060704     C                   MOVEL     §OGNTW        iticlnantw
008600060704     c                   endif
008700060704     c                   endif
008800060704     c
008900060704     C* PRENDO LA VALUTA DAL DESTINATARIO
009000060704     C                   MOVEL     '15'          COD
009100060704     C                   MOVEL(P)  iticnzd       KEY
009200060704     C                   EXSR      CTRTAB
009300060704     C  N32              MOVEL     TBLUNI        DS15
009400060704     C   32              CLEAR                   DS15
009500060704     C                   MOVEL     §15VLT        DESVLT            3
009600060704     c
009700060704     c                   endif
009800060704     c
009900060704     c                   ENDSR
010000011011      *----------------------------------------------------
010100060704      * controlli
010200011011      *----------------------------------------------------
010300060704     c     controlla     begsr
010400060704     C***
010500060704     c*** T I P O     I N C A S S O
010600060704     C***
010700060704     C                   MOVEL     '1A'          COD
010800060704     C* RICERCA
010900060704     C     '?'           SCAN      itictic                                90
011000060704     C*
011100060704    1C     *IN90         IFEQ      *ON
011200060704     C                   MOVEL     'T'           oticric
011300060704     C                   ELSE
011400060704     C                   MOVEL(P)  itictic       KEY
011500060704    1C                   ENDIF
011600060704     C*
011700060704     C                   EXSR      CTRTAB
011800060704     C*
011900060704    1C     oticric       IFEQ      'T'
012000060704     C                   MOVEL     §KEY          otictic
012100060705     C                   GOTO      ENDCTR
012200060704   X1C                   ELSE
012300060704     C*
012400060704    2C     *IN32         IFEQ      *ON
012500060704     C                   MOVEL     MSG(1)        oticmsg
012600060705     c                   movel     'T'           oticerr
012700060704     C                   GOTO      ENDCTR
012800060704    2C                   ENDIF
012900060704     c*
013000060704     c                   movel     itictic       otictic
013100060704    1C                   ENDIF
013200060704     C*
013300060704     C                   MOVEL     TBLUNI        DS1A
013400060705     c
013500060705     c* Se bolla partenza deve essere utilizzabile in partenza
013600090625    1c                   if        itictbo='P' and §1afg1<>'1'
013700060705     C                   MOVEL     MSG(11)       oticmsg
013800060705     c                   movel     'T'           oticerr
013900060705     C                   GOTO      ENDCTR
014000090625    1C                   ENDif
014100060704     C*
014200060704     C*  Aggancio comunque la tabella TM per verificare se abilitazioni
014300060704     c*  oppure forzature
014400060704     C                   MOVEL     'TM'          COD
014500060704     C                   MOVEL(P)  iticccm       KEY
014600060704     C*
014700060704     C                   EXSR      CTRTAB
014800060704     C  n32              MOVEL     TBLUNI        DSTM
014900060704     c   32              clear                   dstm
015000060704     C*
015100090706     c* richiamo pgm per reperimento eventuale forzatura tipo
015200090706     c* incasso per destinatario particolare
015300090706     c                   clear                   tisit0ds
015400090706     c                   clear                   tisit0x1ds
015500090706     c                   movel     'E'           it0tla
015600090706     c                   movel     iticnzd       it0naz
015700090706     c                   movel     iticprd       it0prv
015800090706     c                   movel     iticcad       it0cap
015900090706     c                   movel     iticlod       it0loc
016000090706     c                   movel     iticind       it0ind
016100090706     c                   movel     iticrsd       it0rag
016200090706     c                   call      'TISIT5R'
016300090706     c                   parm                    tisit0ds
016400090706     c                   parm                    tisit0x1ds
016500060704     C* SE devo controllare la Tabella TM dal tipo incasso,
016600060704     c*  vedo se cliente autorizzato
016700060704     c
016800090625     c* Non effettuo il controllo se richiamo da FNLS01R - imm.bolle
016900090625     c*  con ultima posizione del campo PGM = '1'
017000090625     c*  (si tratta dell'indicatore 44 ovvero del richiamo dalle
017100090625     c*   giacenze che non deve effettuare questi controlli per
017200090625     c*   creazione di bolla legata)
017300120320     c* 20/03/2012 : Se richiamato da fnls01r controllo sempre su tabella "TM"
017400120320     c*              se filiale
017500120326   1ac***                if        %subst(iticpgm:1:7)<> 'FNLS01R' or
017600120326     c***                          %subst(iticpgm:10:1)<>'1' or
017700120326     c***                          (%subst(iticpgm:1:7)='FNLS01R' and iticlnp=5)
017800090625    1c                   if        §1ATTM='S'
017900060704     c*
018000060704    2C     *IN32         IFEQ      *ON
018100071130     C                   CLEAR                   tibs02ds
018200071130     c
018300071130    3c                   if        otictic ='SC'
018400071130     c                   movel     'L'           T02tla
018500071130     C                   MOVEL     'C'           T02MOD
018600071130     C                   MOVEL     'FSC'         T02COD
018700071130     c                   movel(P)  iticlna       t02ke1
018800071130     C                   CALL      'TIBS02R'
018900071130     C                   PARM                    KPJBA
019000071130     C                   PARM                    tibs02ds
019100071130    3c                   endif
019200090706    3C                   IF        ((T02ERR <>*BLANKS or t02atb<>*blanks) and
019300120801     c                               (OT0X1TICF=*blanks or OT0X1TICF<>otictic
019400120801     c                                or OT0ERR<>' '))
019500090706     c                             or (otictic<>'SC' and
019600120801     c                              (OT0X1TICF=*blanks or OT0X1TICF<>otictic
019700120801     c                                or OT0ERR<>' '))
019800100208     c* Tipo incasso "SC" inseribile in arrivo sempre solo se in bolla non c'e' alcun tipo incasso C/ASS
019900100208    4c                   if        otictic<>'SC' or itictbo='P'
020000111028    4c                                           or (iticticbol<>'  '
020100160708     c                                           and iticticbol<>'SC' and
020200160708     c                                           %subst(iticpgm:1:7)<>'FNLRG2R')
020300100208    4c                   if        otictic<>'SC'
020400060704     C                   MOVEL     MSG(2)        oticmsg
020500100208     c                   else
020600100208     C                   MOVEL     MSG(13)       oticmsg
020700100208     c                   endif
020800091211     c                   movel     'C'           oticerr
020900060704     C                   GOTO      ENDCTR
021000100208    4C                   ENDIF
021100100208    3C                   ENDIF
021200100208     c
021300071130   x2c                   else
021400060704     C*
021500060704     C* TIC = 'TM'
021600060704     C                   SELECT
021700071130    3C     otictic       WHENEQ    'TM'
021800071130    4C     §TMFTM        IFNE      'S'
021900060704     C                   SETON                                        32
022000071130    4C                   END
022100060704     C* TIC = 'BB'
022200071130    3C     otictic       WHENEQ    'BB'
022300060704     C     otictic       OREQ      'CB'
022400071130    4C     §TMFBB        IFNE      'S'
022500060704     C     §TMFBC        OREQ      'S'
022600060704     C                   SETON                                        32
022700071130    4C                   END
022800060704     C* TIC = 'BC'
022900071130    3C     otictic       WHENEQ    'BC'
023000071130    4C     §TMFBC        IFNE      'S'
023100060704     C                   SETON                                        32
023200071130    4C                   END
023300060704     C* TIC = 'BA'
023400071130    3C     otictic       WHENEQ    'BA'
023500071130    4C     §TMFBA        IFNE      'S'
023600060704     C                   SETON                                        32
023700071130    4C                   END
023800060704     C* TIC = 'CA'
023900071130    3C     otictic       WHENEQ    'CA'
024000071130    4C     §TMFCA        IFNE      'S'
024100060704     C                   SETON                                        32
024200071130    4C                   END
024300090617     C* TIC = 'BP'
024400090617    3C     otictic       WHENEQ    'BP'
024500090617    4C     §TMFBP        IFNE      'S'
024600090617     C                   SETON                                        32
024700090617    4C                   END
024800150520     C* TIC = 'TP'
024900150520    3C     otictic       WHENEQ    'TP'
025000150520    4C     §TMFTP        IFNE      'S'
025100150520     C                   SETON                                        32
025200150520    4C                   END
025300060704     C* TIC = 'SC'
025400071130    3C     otictic       WHENEQ    'SC'
025500071130    4C     §TMFSC        IFNE      'S'
025600071130     c* Verifico anche tabella FSC  forzatura per lna
025700071130     C                   CLEAR                   tibs02ds
025800071130     c                   movel     'L'           T02tla
025900071130     C                   MOVEL     'C'           T02MOD
026000071130     C                   MOVEL     'FSC'         T02COD
026100071130     c                   movel(P)  iticlna       t02ke1
026200071130     C                   CALL      'TIBS02R'
026300071130     C                   PARM                    KPJBA
026400071130     C                   PARM                    tibs02ds
026500100205     c
026600071130    5C                   IF        T02ERR <>*BLANKS or  t02atb<>*blanks
026700100205     c
026800100205     c* Tipo incasso "SC" inseribile in arrivo sempre solo se in bolla non c'e' alcun tipo incasso C/ASS
026900160708    1c                   if        itictbo='P' or (iticticbol<>'  ' and
027000160720     c                                           iticticbol<>'SC' and
027100160708     c                                           %subst(iticpgm:1:7)<>'FNLRG2R')
027200060704     C                   SETON                                        32
027300100205    5C                   ENDif
027400100205    5C                   END
027500071130    4C                   END
027600060704     C* TIC = 'TO'
027700071130    3C     otictic       WHENEQ    'TO'
027800071130    4C     §TMFTO        IFNE      'S'
027900060704     C                   SETON                                        32
028000071130    4C                   END
028100080625     C* TIC = 'S2'
028200080625    3C     otictic       WHENEQ    'S2'
028300080625    4C     §TMFS2        IFNE      'S'
028400080625     C                   SETON                                        32
028500080625    4C                   END
028600071130    3C                   ENDSL
028700060704     C*
028800090706    4C***  *IN32         IFEQ      *ON
028900090706     c                   if        *in32 and
029000120802     c                             (OT0X1TICF=*blanks or OT0X1TICF<>otictic
029100120802     c                             or OT0ERR<>' ')
029200060704     C                   MOVEL     MSG(2)        oticmsg
029300091211     c                   movel     'C'           oticerr
029400060704     C                   GOTO      ENDCTR
029500071130    4C                   END
029600071130    2C                   END
029700060704     c
029800060704    1c                   endif
029900120326   1ac******             endif
030000121022     c*
030100121022     c* Se spedizione export controllo che il tipo incasso sia gestito dal partner
030200121022     c                   if        (iticlnantw='DPD' or iticlnantw='FED' or
030300121022     c                                 iticlnantw='EEX')
030400121022     c                             and %subst(iticpgm:1:7)='FNLS01R'
030500121022     C                   CLEAR                   tibs02ds
030600121022     c                   movel     'L'           T02tla
030700121022     C                   MOVEL     'C'           T02MOD
030800121022     C                   MOVEL     '1AX'         T02COD
030900121022     c                   movel(P)  iticlna       t02ke1
031000121022     c                   movel(P)  otictic       t02ke2
031100121022     C                   CALL      'TIBS02R'
031200121022     C                   PARM                    KPJBA
031300121022     C                   PARM                    tibs02ds
031400121022     c                   if        t02err<>*blanks or t02atb<>*blanks
031500121022     C                   MOVEL     MSG(14)       oticmsg
031600121023     c                   eval      oticmsg=%trim(oticmsg)+' ' +
031700121023     c                                     %editc(iticlna:'X')
031800121022     c                   movel     'C'           oticerr
031900121022     c                   endif
032000121022     c                   endif
032100060704     c
032200120802     c* Verifico se ha forzature in manutenzione: se tipo incasso forzabile
032300060704     c*  in partenza, non modificabile
032400160908     c*  se il tipo incasso è quello forzato. E' capitato infatto che un cliente
032500160908     c*  avesse in bolla CM e non potevano mettere SC perchè lui aveva la frozatura, ma per SC non per CM
032600060704    1c                   if        §tmfti<>'  ' and otictic<>iticTICbol and
032700160908     c                             iticticbol<>*blanks   and iticticbol=§tmfti
032800121022     c                             and iticlnantw<>'DPD'
032900121022     c                             and iticlnantw<>'EEX'
033000121022     c                             and iticlnantw<>'FED'
033100170623     c
033200170623     c                   eval      werr='S'
033300170623      * Verifico il limite del contate: se supera non do errore
033400170623    2c                   if        %subst(iticpgm:1:7)=  'FNLRG2R'
033500170623     c                   clear                   dlca
033600170623     c                   clear                   tibs02ds
033700170623     c                   movel     'C'           t02mod
033800170623     c                   movel     knsif         t02sif
033900170623     c                   movel     'LCA'         t02cod
034000170623     c                   movel(p)  '017'         t02ke1
034100170623     C                   CALL      'TIBS02R'
034200170623     C                   PARM                    KPJBA
034300170623     C                   PARM                    tibs02ds
034400170623    3c                   if        t02err= *blanks and t02atb= *blanks
034500170623     c                   eval      dlca=t02uni
034600170623    4c                   if        iticcas>§LCACO
034700170623     c                   eval      werr='N'
034800170623    4c                   endif
034900170623    3c                   endif
035000170623    2c                   endif
035100170623
035200170623    2c                   if        werr='S'
035300060704     C                   MOVEL     MSG(3)        oticmsg
035400091211     c                   movel     'C'           oticerr
035500060704     C                   GOTO      ENDCTR
035600170623    2C                   ENDif
035700170623    1C                   ENDif
035800120802     c                   if        ot0err=' ' and
035900120802    1c                             OT0X1TICF<>*blanks and itictbo='A' and
036000090706     c                             otictic<>iticticbol and
036100090706     c                             iticticbol<>*blanks
036200121022     c                             and iticlnantw<>'DPD'
036300121022     c                             and iticlnantw<>'EEX'
036400121022     c                             and iticlnantw<>'FED'
036500090706     C                   MOVEL     MSG(12)       oticmsg
036600090706     c                   movel     'T'           oticerr
036700090706     C                   GOTO      ENDCTR
036800090706    1C                   ENDif
036900160510     c
037000160510     c                   if        %subst(iticpgm:1:7)=  'FNLRG2R'
037100160510     c                             and  itictic=*blanks
037200160510     C                   MOVEL     MSG(16)       oticmsg
037300160510     c                   movel     'T'           oticerr
037400160510     C                   GOTO      ENDCTR
037500160510     c                   endif
037600160510     c* se tipo incasso "SOLO CONTANTE" non è accettabile un importo > del limite previsto
037700160510     c*  di legge. Tale limite lo prendo dal record 017 solo per contatti DEST
037800160510     c
037900160510     c                   if        %subst(iticpgm:1:7)=  'FNLRG2R'
038000160510     c                             and §1afca='S'
038100160510     c                   clear                   dlca
038200160510     c                   clear                   tibs02ds
038300160510     c                   movel     'C'           t02mod
038400160510     c                   movel     knsif         t02sif
038500160510     c                   movel     'LCA'         t02cod
038600160510     c                   movel(p)  '017'         t02ke1
038700160510     C                   CALL      'TIBS02R'
038800160510     C                   PARM                    KPJBA
038900160510     C                   PARM                    tibs02ds
039000160510     c                   if        t02err= *blanks and t02atb= *blanks
039100160510     c                   eval      dlca=t02uni
039200160510     c                   if        iticcas>§LCACO
039300160510     C                   MOVEL     MSG(15)       oticmsg
039400160510     c                   movel     'T'           oticerr
039500160510     C                   GOTO      ENDCTR
039600160510     c                   endif
039700160510     c                   endif
039800160510     c                   endif
039900060704     C***
040000060704     C***   D I V I S A
040100060704     C***
040200060704      * La divisa è obbligatoria se è inserito un importo
040300060704    1C     ITICCAS       IFGT      *ZEROS
040400060704     C     ITICdiv       ANDEQ     *BLANKS
040500060704     C                   MOVEL     MSG(4)        OTICMSG
040600060705     c                   movel     'D'           oticerr
040700060704     C                   GOTO      ENDCTR
040800060704    1C                   ENDIF
040900060704     C*
041000060704     C                   MOVEL     'CV'          COD
041100060704     C* RICERCA
041200060704     C     '?'           SCAN      iticdiv                                90
041300060704    1C     *IN90         IFEQ      *ON
041400060704     C                   MOVEL     'D'           oticric
041500060704   x1C                   ELSE
041600060704     C                   MOVEL(P)  iticdiv       KEY
041700060704    1C                   ENDIF
041800060704     C*
041900060704     C                   EXSR      CTRTAB
042000060704     C*
042100060704    1C     oticric       IFne      ' '
042200060704     C                   MOVEL     §KEY          oticdiv
042300060705     C                   GOTO      ENDCTR
042400060704   X1C                   ELSE
042500060704     C* ERRORE
042600060704    2C     *IN32         IFEQ      *ON
042700060704     C                   MOVEL     MSG(5)        oticmsg
042800060705     c                   movel     'D'           oticerr
042900060704     C                   GOTO      ENDCTR
043000060704    2C                   ENDIF
043100060704     c
043200060704     c                   movel     iticdiv       oticdiv
043300060704    1C                   ENDIF
043400060704     c
043500060704     C                   MOVEL     TBLUNI        DSCV
043600060704     C*
043700060704     C* VEDO SE AMMESSI IMPORTI DECIMALI NELL'IMPORTO
043800060704    1C     §CVFDC        IFNE      'S'
043900060704     C                   MOVE      iticcas       W0033             3 3
044000060704    2C     W0033         IFNE      0
044100060704     C                   MOVEL     MSG(6)        oticmsg
044200060705     c                   movel     'D'           oticerr
044300060704     C                   GOTO      ENDCTR
044400060704    2C                   ENDIF
044500060704    1C                   ENDIF
044600060704      *
044700060704      * Controllo il numero di decimali immessi
044800060704    1C     §CVFDC        IFEQ      'S'
044900060704     C     iticcas       ANDgt     *ZEROS
045000060704      *
045100060704    2C                   SELECT
045200060704     C     §CVDEC        WHENEQ    3
045300060704     C     §CVDEC        WHENEQ    2
045400060704     C                   MOVE      iticcas       W0010             1 0
045500060704    3C     W0010         IFNE      *ZEROS
045600060704     C                   MOVEA     MSG(7)        oticmsg
045700060704     c                   eval      %subst (oticmsg:29:2) = (%editc(§cvdec:'X'))
045800060705     c                   movel     'D'           oticerr
045900060704    3C                   ENDIF
046000060704    c
046100060704     C     §CVDEC        WHENEQ    1
046200060704     C                   MOVE      iticcas       W0020             2 0
046300060704    3C     W0020         IFNE      *ZEROS
046400060704     C                   MOVEA     MSG(7)        oticmsg
046500060704     c                   eval      %subst (oticmsg:29:2) = (%editc(§cvdec:'X'))
046600060705     c                   movel     'D'           oticerr
046700060704    3C                   ENDIF
046800060704    c
046900060704     C     §CVDEC        WHENEQ    0
047000060704    2C                   ENDSL
047100060704     c
047200060705    2c                   if        oticerr<>' '
047300060704     c                   goto      endctr
047400060704    2c                   endif
047500060704      *
047600060704    1C                   ENDIF
047700060704     C*
047800060704     C* SE DIVISA ESTERA VEDO SE POSSO ACCETTARE un TIPO INCASSO
047900060704     c*    non previsto per l'estero
048000060704    1C     §CVDVA        IFNE      'S'
048100060704     C     §1AUDE        ANDNE     'S'
048200060704     C                   MOVEL     MSG(8)        oticmsg
048300060705     c                   movel     'T'           oticerr
048400060704     C                   GOTO      ENDCTR
048500060704    2C                   END
048600060704      *
048700060704      * reperisco dati tabella 'CW'
048800060704     C                   CLEAR                   DSCW
048900060704     C                   MOVEL     'CW'          COD
049000060704     C                   MOVEL(P)  oticdiv       KEY
049100060704     C                   EXSR      CTRTAB
049200060704     C     *IN32         IFEQ      *OFF
049300060704     C                   MOVEL     TBLUNI        DSCW
049400060704     C                   ENDIF
049500060704     c
049600060704      * Controllo se la divisa immessa è valida per il tipo incasso
049700060704     C     §1ACDL        IFEQ      'S'
049800060704     C     iticdsp       ANDGT     §CWDLC
049900060704     C                   MOVEL     MSG(9)        oticmsg
050000060705     c                   movel     'D'           oticerr
050100060704     C                   GOTO      ENDCTR
050200060704     C                   ENDIF
050300060704     C*
050400060704     C* PER EXPORT BARTOLINI, DPD o FEDEX NON POSSO MODIFICARE LA DIVISA
050500060704     c*  al massimo l'eur
050600060704     C     oticdiv       IFNE      DESVLT
050700060704     C     oticdiv       ANDNE     'EUR'
050800060704     c     iticlnantw    ifeq      'EEX'
050900060704     c     iticlnantw    oreq      'DPD'
051000060704     c     iticlnantw    oreq      'FED'
051100060704     C                   MOVEL     MSG(10)       oticmsg
051200060705     c                   movel     'D'           oticerr
051300060704     C                   GOTO      ENDCTR
051400060704    2C                   ENDIF
051500060704     c                   ENDIF
051600011106
051700060704     c     ENDCTR        ENDSR
051800060704     c*
051900060704     C* RICERCA E CONTROLLO RECORD IN TABELLA ------------------------*
052000060704     C     CTRTAB        BEGSR
052100060704     C*  RICERCA
052200060704     C     oticric       IFne      ' '
052300060704     C                   MOVEL     COD           §COD
052400060704     C                   CLEAR                   §KEY
052500060704     C                   CALL      'X§TABER'
052600060704     C                   PARM                    §KUT
052700060704     C                   PARM                    §COD
052800060704     C                   PARM                    §KEY
052900060704     C                   PARM                    §DES             30
053000060704     C                   SETON                                        90
053100060704     C*
053200060704     C                   ELSE
053300060704     C*
053400060704     c* controllo
053500060704     C     KTAB          CHAIN     TABEL                              32
053600060704     C  N32TBLFLG        IFNE      ' '
053700060704     C                   SETON                                        32
053800060704     C                   END
053900060704     C                   END
054000060704     c
054100060704     C                   ENDSR
054200060704**         MSG
054300060704Tipo incasso C/Assegno errato                                                 01
054400060704Cliente non autorizzato al tipo incasso C/A indicato                          02
054500060704Tipo incasso non modificabile per questo cliente                              03
054600060704Divisa C/Assegno obbligatoria se presente importo                             04
054700060704Valuta C/Assegno errata                                                       05
054800060704Non ammessi importi con decimali per la valuta C/Assegno indicata             06
054900060704Ammessi importi con massimo __ decimali per la divisa indicata                07
055000060704Tipo incasso non utilizzabile se C/Assegno in divisa estera                   08
055100060704Tipo incasso non utilizzabile per la divisa del Contrassegno                  09
055200060704Bolla EXPORT: la divisa del C/A deve essere o quella del destinatario o "EUR" 10
055300060705Tipo Incasso C/Assegno inesistente o non e' da indicare in partenza           11
055400090706Tipo incasso non modificabile per questo destinatario                         12
055500100208Tipo incasso "SC" non ammesso per questa spedizione                           13
055600121023Spedizione export: Tipo incasso non abilitato per la linea di arrivo          14
055700160510Importo C/Assegno supera il limite previsto di legge per i CONTANTI           15
055800160510Obbligatorio indicare il tipo incasso C/Assegno                               16
