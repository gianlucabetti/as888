000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800060919     Ftitvu8p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100060919     Ftitvu8ps  O    f  198        PRINTER usropn
001200000621     F                                     oflind(*inob)
001300070411     F                                     infsr(*pssr)
001400990908
001500000512     D*------------
001600000512     D* COMANDI
001700000512     D*------------
001800011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001900000801     D*----------------------------------------------------
002000000801     D* DICHIARAZIOINE VARIABILI DI WRK
002100000801     D*----------------------------------------------------
002200990920     D dscmz         e ds                  inz
002300990910     D psds           sds
002400990910     D  procname         *PROC
002500990920     D tivlrds       e ds                  extname(tivlr00f)
002600990910     D esito           s              1
002700000724     D prmlit          s             10
002800000710     D prmfir          s             10
002900990921     D wrkesito        s                   like(esito)
003000990915     D wrkdata         s               d
003100990915     D wrkora          s               t
003200000613     D rrnum           s              6  0 INZ(*zeros)
003300000621     D recko           s            150    INZ(*blanks)
003400011113     D depcmd          s            150    INZ(*blanks)
003500010202     D parccm          s              8    INZ(*blanks)
003600010202     D parmbr          s             10    INZ(*blanks)
003700010202     D paropz          s              1    INZ(*blanks)
003800010202     D chkcall         s              1    INZ(*blanks)
003900060919     D wGiro           s              1  0 INZ(*zeros)
004000000830
004100000830     D*------------------
004200000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004300000830     D*------------------
004400000830     D WLBDA8          DS                  INZ
004500000830     D  G08DAT                 1      8  0
004600000830     D  G08INV                 9     16  0
004700000830     D  G08ERR                17     17
004800000830     D  G08TGI                18     22  0
004900990908
005000010201
005100010201
005200990915     C                   time                    wrkdata
005300990915     C                   time                    wrkora
005400000913     C                   reset                   rrnum
005500990921     C                   reset                   esito
005600990921     C                   reset                   wrkesito
005700000724     C*
005800000724     C* SE OCCORRE SPEDIRE IN FILIALE
005900000724     C                   if        vlrpoi <> *zeros
006000000724     C*
006100000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
006200000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
006300000724     C     vlrpoi        chain     azorg01l
006400000724     C                   if        %found
006500000616     C                   movel(p)  CMD(1)        depcmd
006600020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
006700000616     C*
006800000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
006900011113     C                   Z-ADD     150           LENGH            15 5
007000000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
007100000616     C                   PARM                    depcmd
007200000616     C                   PARM                    LENGH
007300000724     C*
007400000724     C                   endif
007500000724     C                   endif
007600000616     C*
007700000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007800000616     C                   movel(p)  CMD(2)        depcmd
007900000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
008000000616     C*
008100000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
008200011113     C                   Z-ADD     150           LENGH            15 5
008300000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008400000616     C                   PARM                    depcmd
008500000616     C                   PARM                    LENGH
008600000616     C*
008700060919     C                   if        not %open(titvu8ps)
008800060919     C                   open      titvu8ps
008900000616     C                   except    testdett
009000000613     C                   endif
009100000613     C*
009200040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
009300000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
009400000613     C*
009500010202     C* Effettuo la chiamata al CLLE preposto
009600040506     C                   call(e)   'TITVVTC'
009700010202     C                   parm                    parccm
009800010202     C                   parm                    parmbr
009900010202     C                   parm      '2'           paropz
010000010202     C*
010100060919     C                   if        %open(titvu8ps)
010200000616     C                   except    findett
010300060919     C                   close     titvu8ps
010400000613     C                   endif
010500000616     C*
010600000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
010700011113     C                   Z-ADD     150           LENGH            15 5
010800000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010900000616     C                   PARM                    CMD(3)
011000000616     C                   PARM                    LENGH
011100000616     C*
011200010201     C                   seton                                        LR
011300000613
011400000613
011500000613     C*--------------------------------------------------------
011600000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
011700000613     C*--------------------------------------------------------
011800000621     C     STPR          BEGSR
011900000613     C*
012000060919     C                   if        not %open(titvu8p)
012100060919     C                   open      titvu8p
012200990915     C                   endif
012300990915     C*
012400990915     C                   except    riepilogo
012500990915     C*
012600060919     C                   if        %open(titvu8p)
012700060919     C                   close     titvu8p
012800990914     C                   endif
012900990910     C*
013000000613     C                   ENDSR
013100000613     C***
013200990908
013300000801
013400910830     C*--------------------------------------------------------
013500040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
013600910830     C*--------------------------------------------------------
013700040526     C     RWFILE        BEGSR
013800990910     C*
013900990914     C                   if        not %open(tivin00r)
014000990908     C                   open      tivin00r
014100990914     C                   endif
014200021113     C                   if        not %open(fivabwwr)
014300021113     C                   open      fivabwwr
014400990914     C                   endif
014500021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
014600020305     C                   exsr      prevat
014700010201     C*
014800010202     C                   if        chkcall = '0'
014900010202     C*
015000021113     C                   if        not %open(fivatwwr)
015100021113     C                   open      fivatwwr
015200010201     C                   endif
015300990910     C*
015400010201     C                   clear                   §CTROKVB          5 0
015500020305     C                   clear                   §CTROKVT          5 0
015600000801     C                   clear                   §CTRMO            5 0
015700000801     C                   clear                   §CTRNO            5 0
015800040910     C*
015900921023     C                   DO        *HIVAL
016000990913     C*
016100990915     C                   READ      tivin00r                               70
016200040910     C                   if        vindta > *blanks
016300000613     C                   add       1             rrnum
016400000801     C*
016500000801     C                   if        *in70 = *off
016600000801     C                             and
016700000801     C                             (vinflg = *blanks
016800000801     C                              or vinflg = '0'
016900000801     C                              or vinflg = '2')
017000000801     C*
017100000801     C                   clear                   vinmsg
017200000801     C                   eval      vinflg = '1'
017300040910     C*
017400040910     C* Eseguo routine d traduzione
017500040910     C                   exsr      impvabvat
017600040802     C*
017700010305     C                   endif
017800000905     C*
017900000905     C                   else
018000000905     C                   eval      vinflg = '1'
018100000905     C                   endif
018200000905     C*
018300000905     C  N70              update    tivin000
018400000905     C*
018500991022     C  N70              ENDdo
018600070918     C*
018700070918     C* A fine file scarico l'ultimo record rimasto in canna
018800071029     C                   exsr      wrivab
018900010202     C*
019000010202     C                   endif
019100990910
019200990910     C* Se non ci sono record con errori ...
019300000710     C                   if        §ctrno = 0
019400990910     C* ... restituisco esito OK.
019500990921     C                   eval      wrkesito = '0'
019600990910     C                   else
019700010201     C                   if        §ctrokvb > 0
019800990921     C                   eval      wrkesito = '1'
019900000710     C                   else
020000000710     C                   eval      wrkesito = '2'
020100990910     C                   endif
020200000710     C                   endif
020300990910     C*
020400990914     C                   if        %open(tivin00r)
020500990908     C                   close     tivin00r
020600990914     C                   endif
020700021113     C                   if        %open(fivabwwr)
020800021113     C                   close     fivabwwr
020900990914     C                   endif
021000021113     C                   if        %open(fivatwwr)
021100021113     C                   close     fivatwwr
021200010201     C                   endif
021300990910     C*
021400010201     C                   if        §ctrokvb > 0
021500000724     C                             and vlrpoi <> *zeros
021600010202     C                   exsr      invio
021700990920     C                   endif
021800990920     C*
021900910830     C                   ENDSR
022000000613     C***
022100990920
022200000801     C*----------------------------------------------------*
022300000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
022400000801     C*----------------------------------------------------*
022500010201     C     INZVAR        BEGSR
022600000801     C*
022700040802     C                   Z-ADD     *zeros        Num5_0            5 0
022800040802     C                   MOVEL     '0'           FlgCAS            1
022900000801     C*
023000000801     C                   ENDSR
023100000801     C*----------------------------------------------------*
023200040910     C*  IMPOSTAZIONE CAMPI COSTANTI
023300000801     C*----------------------------------------------------*
023400000801     C     DEFCAM        BEGSR
023500000801     C*
023600021113     C                   CLEAR                   FIVAB000
023700040802     C                   CLEAR                   FIVAT000
023800020619     C* Imposto i valori di default...
023900060919     C                   Z-ADD     0630138       VABCCM
024000060919     C                   Z-ADD     0630138       VATCCM
024100060919     C                   Z-ADD     063           VABLNP
024200060919     C                   Z-ADD     063           VATLNP
024300040714     C                   Z-ADD     000           VABCTR
024400040823     C                   MOVEL     '1'           VABCBO
024500060206     C                   MOVEL     'D'           VABTSP
024600020619     C* ... e poi verifico se sono stati passati come parametri
024700020619     C                   IF        vlrppt > *blanks
024800040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
024900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
025000020619     C                   EXSR      CHKNUM
025100020619     C                   IF        PiInt=*on
025200020619     C                   Z-ADD     PiVal         VABCCM
025300020619     C                   Z-ADD     PiVal         VATCCM
025400020619     C                   ENDIF
025500040506     C                   ENDIF
025600040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
025700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
025800020619     C                   EXSR      CHKNUM
025900020619     C                   IF        PiInt=*on
026000020619     C                   Z-ADD     PiVal         VABLNP
026100020619     C                   Z-ADD     PiVal         VATLNP
026200040506     C                   ENDIF
026300020619     C                   ENDIF
026400040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
026500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
026600020619     C                   EXSR      CHKNUM
026700020619     C                   IF        PiInt=*on
026800020619     C                   Z-ADD     PiVal         VABCTR
026900040506     C                   ENDIF
027000020619     C                   ENDIF
027100020619     C                   ENDIF
027200000801     C*
027300000801     C                   ENDSR
027400000801     C*----------------------------------------------------*
027500040910     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB e FIVAT)
027600000801     C*----------------------------------------------------*
027700040910     C     IMPVABVAT     BEGSR
027800040910     C*
027900040910     C* Traduzione relativa ai tipi record del file del cliente
028000040910     C*
028100040910     C***
028200060919     C* ...tipo record 01
028300060919     C                   IF        %subst(vindta:1:2) = '01'
028400060919     C                   IF        wGiro = 1
028500060919     C*
028600060919     C* Considerazioni sul contenuto di campi precedentemente valorizzati
028700060919     C                   IF        FlgCAS <> '0'
028800060919     C                   IF        VABCBO = '1'
028900060919     C                   EVAL      VABCBO = '4'
029000060919     C                   ELSE
029100060919     C                   EVAL      VABCBO = '6'
029200060919     C                   ENDIF
029300060919     C                   ENDIF
029400061025     C*
029500061025     C* Effettuo determinate considerazioni in base alle caratteristiche della bolla x determinare
029600061025     C* il codice cliente mittente e la tariffa
029700061025     C                   IF        VABCBO='1' AND VABNZD=*blanks
029800061025     C                   EVAL      VABCCM=0630138
029900061031     C                   EVAL      VATCCM=0630138
030000061025     C                   EVAL      VABCTR=000
030100061025     C                   ENDIF
030200061025     C*
030300061025     C                   IF        FlgCAS<>'0' AND VABNZD=*blanks
030400061025     C                   EVAL      VABCCM=0631418
030500061031     C                   EVAL      VATCCM=0631418
030600061025     C                   EVAL      VABCTR=000
030700061218     C                   MOVEL     'A'           VABTC1
030800061025     C                   ENDIF
030900061025     C*
031000061025     C                   IF        VABNZD<>*blanks
031100061025     C                   EVAL      VABCCM=0631418
031200061031     C                   EVAL      VATCCM=0631418
031300061025     C                   EVAL      VABCTR=050
031400061218     C                   MOVEL     'A'           VABTC1
031500061025     C                   ENDIF
031600060919     C*
031700060919     C* Eseguo routine finale x considerazioni specifiche su importi/divise
031800061025     C                   EXSR      CHKIMPDIV
031900060919     C*
032000060919     C* ...e in fine scarico il buffer d output
032100071029     C                   EXSR      wrivab
032200060919     C                   ELSE
032300060919     C                   Z-ADD     1             wGiro
032400060919     C                   ENDIF
032500060919     C*
032600040910     C* ......inizializzazioni iniziali e formati record file Bartolini
032700040910     C                   EXSR      INZVAR
032800040910     C                   EXSR      DEFCAM
032900040928     C*
033000040928     C                   Z-ADD     *zeros        errore            1 0
033100040928     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
033200040928     C                   MOVEL     datcor        VABAAS
033300040928     C                   MOVEL     datcor        VATAAS
033400040928     C                   MOVE      datcor        VABMGS
033500040928     C                   MOVE(P)   vlrpoi        VABFGS
033600040928     C                   MOVE(P)   vlrpoi        VATFGS
033700060919     C* ......VABNSP/VATNSP/VABRMN
033800060919     C                   EVAL      PiStr=%trim(%subst(vindta:4:9))
033900060919     C                   EXSR      CHKNUM
034000060919     C                   IF        PiInt=*on
034100060919     C                   Z-ADD     PiVal         VABNSP
034200060919     C                   Z-ADD     PiVal         VATNSP
034300060919     C                   Z-ADD     PiVal         VABRMN
034400060919     C                   ELSE
034500060919     C                   ADD       1             errore
034600060919     C                   EVAL      vinmsg = %trimr(vinmsg)
034700060919     C                             + ' ' + 'VABNSP/VATNSP/VABRMN'
034800060919     C                   ENDIF
034900060919     C* ......VABNAS
035000060919     C                   EVAL      VABNAS=%trim(%subst(vindta:19:20))
035100060919     C* ......VABRSD/VABRD2
035200060919     C                   MOVEL     *blanks       wDEST            70
035300061031     C                   EVAL      wDEST=%trim(%subst(vindta:53:35)) + ' '+
035400060919     C                                   %trim(%subst(vindta:213:35))
035500061018     C                   EVAL      VABRSD=%trim(%subst(wDEST:1:35))
035600061018     C                   EVAL      VABRD2=%trim(%subst(wDEST:36:35))
035700060919     C* ......VABNCL
035800060919     C                   EVAL      PiStr=%trim(%subst(vindta:41:4))
035900060919     C                   EXSR      CHKNUM
036000060919     C                   IF        PiInt=*on
036100060919     C                   ADD       PiVal         VABNCL
036200060919     C                   ELSE
036300060919     C                   ADD       1             errore
036400060919     C                   EVAL      vinmsg = %trimr(vinmsg)
036500060919     C                             + ' ' + 'VABNCL'
036600060919     C                   ENDIF
036700040910     C*
036800040910     C                   ENDIF
036900040910     C***
037000060919     C* ...tipo record 02
037100060919     C                   IF        %subst(vindta:1:2) = '02'
037200040910     C* ......VABIND
037300071029     C                   MOVEL     *blanks       wINDIR           55
037400071029     C                   EVAL      wINDIR=%trim(%subst(vindta:3:47))+' '+
037500071022     C                                    %trim(%subst(vindta:271:6))
037600040910     C* ......VABCAD
037700071022     C                   EVAL      VABCAD=%trim(%subst(vindta:48:6))
037800040910     C* ......VABLOD
037900071022     C                   EVAL      VABLOD=%trim(%subst(vindta:54:28))
038000060919     C* ......VABPRD
038100071022     C                   EVAL      VABPRD=%trim(%subst(vindta:82:2))
038200060919     C* ......VABNZD
038300071022     C                   EVAL      VABNZD=%trim(%subst(vindta:84:4))
038400060919     C                   IF        %trim(VABNZD) = 'IT'
038500060919     C                   EVAL      VABNZD=*blanks
038600061018     C                   ENDIF
038700060919     C* ......VABRMA
038800071022     C                   EVAL      VABRMA=%trim(%subst(vindta:188:8))
038900060919     C* ......VABPKB
039000071022     C                   EVAL      PiStr=%trim(%subst(vindta:202:7))
039100060919     C                   EXSR      CHKNUM
039200060919     C                   IF        PiNum=*on
039300060919     C                   EVAL(H)   VABPKB=PiVal/1000                            * gestisco 3 dec.
039400060919     C                   ELSE
039500060919     C                   ADD       1             errore
039600060919     C                   EVAL      vinmsg = %trimr(vinmsg)
039700060919     C                             + ' ' + 'VABPKB'
039800060919     C                   ENDIF
039900060919     C* ......VATNOT "B" - Telefono destinatario
040000071022     C                   EVAL      VATNOT=%trim(%subst(vindta:211:15))+' '+
040100071022     C                                    %trim(%subst(vindta:226:15))
040200060919     C                   IF        VATNOT <> *blanks
040300060919     C                   EVAL      VATTRC = 'B'
040400060919     C                   WRITE     FIVAT000
040500060919     C                   ENDIF
040600040910     C*
040700060919     C                   ENDIF
040800040910     C***
040900061018     C* ...tipo record 05
041000061018     C                   IF        %subst(vindta:1:2) = '05'
041100061018     C* ......VABCAS
041200060919     C                   IF        %subst(vindta:108:9) <> *zeros
041300040910     C                   EVAL      FlgCAS = '1'
041400060919     C                   EVAL      VABVCA='EUR'
041500060919     C                   EVAL      PiStr=%trim(%subst(vindta:108:9))
041600040910     C                   EXSR      CHKNUM
041700040910     C                   IF        PiNum=*on
041800061025     C                   EVAL(H)   VABCAS=PiVal/100                             * gestisco 2 dec.
041900040910     C                   ELSE
042000040910     C                   ADD       1             errore
042100040910     C                   EVAL      vinmsg = %trimr(vinmsg)
042200040910     C                             + ' ' + 'VABCAS'
042300040910     C                   ENDIF
042400061018     C                   ENDIF
042500040910     C*
042600060919     C                   ENDIF
042700040910     C***
042800061018     C* ...tipo record 06
042900061018     C                   IF        %subst(vindta:1:2) = '06'
043000060919     C* ......VABNOT/VABNT2
043100060919     C                   MOVEL     *blanks       wNOTE            70
043200060919     C                   EVAL      wNOTE=%trim(%subst(vindta:3:99))
043300040910     C*
043400040910     C                   ENDIF
043500040910     C***
043600010202     C*
043700000801     C* Ebbene...
043800000801     C                   ADD       1             §CTRMO
043900010201     C                   IF        errore <> *zeros
044000000801     C                   ADD       1             §CTRNO
044100000801     C                   EVAL      recko = vindta
044200000801     C                   EXCEPT    rigadett
044300000801     C                   EVAL      vinflg = '2'
044400000801     C                   ELSE
044500010201     C                   ADD       1             §CTROKVB
044600000801     C                   ENDIF
044700000801     C*
044800000801     C                   ENDSR
044900071029
045000071029     C*----------------------------------------------------*
045100071029     C*  SCARICO BUFFER FILE TESTATE - FNVAB
045200071029     C*----------------------------------------------------*
045300071029     C     WRIVAB        BEGSR
045400071029     C*
045500071029     C* Verifico occupazione campo completo indirizzo
045600071029     C                   if        %len(%trim(wINDIR))>%size(VABIND)
045700071029     C                   eval      VABIND=%subst(%trim(wINDIR):1:%size(VABIND))
045800071029     C                   eval      VABNOT=%subst(%trim(wINDIR):%size(VABIND)+1)
045900071029     C                   eval      VABNT2=%trim(wNOTE)
046000071029     C                   else
046100071029     C                   eval      VABIND=%trim(wINDIR)
046200071029     C                   eval      VABNOT=%subst(wNOTE:1:35)
046300071029     C                   eval      VABNT2=%subst(wNOTE:36:35)
046400071029     C                   endif
046500071029     C*
046600071029     C                   write     FIVAB000
046700071029     C*
046800071029     C                   ENDSR
046900010202     C*----------------------------------------------------*
047000021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
047100010202     C*----------------------------------------------------*
047200020305     C     PREVAT        BEGSR
047300010202     C*
047400021113     C* Compongo il nome del membro da dare al FIVATWWR
047500010202     C                   eval      parmbr = vlrhdl
047600010202     C                   movel     'M'           parmbr
047700060113     C                   eval      parccm = vlrksc
047800010202     C                   eval      paropz = '1'
047900010202     C* Effettuo la chiamata al CLLE preposto
048000040506     C                   call(e)   'TITVVTC'
048100010202     C                   parm                    parccm
048200010202     C                   parm                    parmbr
048300010202     C                   parm                    paropz
048400010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
048500010202     C                   if        %error
048600010202     C                   movel     '1'           chkcall
048700010202     C                   else
048800010202     C                   movel     '0'           chkcall
048900010202     C                   endif
049000010202     C*
049100010202     C                   ENDSR
049200000801     C*----------------------------------------------------*
049300000801     C*  CONTROLLO NUMERICITA' CAMPI
049400000801     C*----------------------------------------------------*
049500000801     C     CHKNUM        BEGSR
049600000801     C*
049700000801     C                   call(e)   'ISNUMERIC'
049800000801     C                   PARM                    PiStr            30
049900040714     C                   PARM      ','           PiDecChr          1
050000000801     C                   PARM      *ZEROS        PiVal            30 9
050100000801     C                   PARM      '0'           PiInt             1
050200000801     C                   PARM      '0'           PiNum             1
050300000801     C                   IF        %error
050400000801     C                   EVAL      PiInt=*off
050500000801     C                   ENDIF
050600000801     C*
050700000801     C                   ENDSR
050800000801     C***
050900000801
051000011113
051100011113     C*----------------------------------------------------*
051200011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
051300011113     C*----------------------------------------------------*
051400011113     C     CHKIMPDIV     BEGSR
051500011113     C*
051600011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
051700011113     C                   Z-ADD     *zeros        wrkDec            9 9
051800011113     C*
051900011113     C* Come prima cosa effettuo considerazioni sulla divisa
052000011113     C                   IF        vabIAS > *zeros
052100011113     C                   IF        vabVAS <> 'EUR'
052200011113     C                   EVAL      vabVAS =  'ITL'
052300011113     C                   ENDIF
052400011113     C                   ENDIF
052500011113     C*
052600011113     C                   IF        vabCAS > *zeros
052700011113     C                   IF        vabVCA <> 'EUR'
052800011113     C                   EVAL      vabVCA =  'ITL'
052900011113     C                   ENDIF
053000011113     C                   ENDIF
053100011113     C*
053200011113     C                   IF        vabVMD > *zeros
053300020305     C                   IF        vabVAD <> 'EUR'
053400011113     C                   EVAL      vabVAD =  'ITL'
053500011113     C                   ENDIF
053600011113     C                   ENDIF
053700011113     C*
053800011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
053900011113     C                   Z-ADD     vabIAS        wrkDec
054000011113     C                   IF        wrkDec > *zeros
054100011113     C                   IF        vabVAS = 'ITL'
054200011113     C                   EVAL      vabIAS = *zeros
054300011113     C                   ENDIF
054400011113     C                   ENDIF
054500011113     C*
054600011113     C* Stabilisco se il contrasegno ha decimali valorizzati
054700011113     C                   Z-ADD     vabCAS        wrkDec
054800011113     C                   IF        wrkDec > *zeros
054900011113     C                   IF        vabVCA = 'ITL'
055000011113     C                   EVAL      vabCAS = *zeros
055100011113     C                   ENDIF
055200011113     C                   ENDIF
055300011113     C*
055400011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
055500011113     C                   Z-ADD     vabVMD        wrkDec
055600011113     C                   IF        wrkDec > *zeros
055700011113     C                   IF        vabVAD = 'ITL'
055800011113     C                   EVAL      vabVMD = *zeros
055900011113     C                   ENDIF
056000011113     C                   ENDIF
056100011113     C*
056200011113     C                   ENDSR
056300011113     C***
056400011113
056500011113
056600000801
056700000801
056800990920      /TITLE Invio dei dati al punto operativo.
056900010202     C     invio         BEGSR
057000990920     C*
057100021113     C* 1° invio FIVAT
057200010201     C                   reset                   dscmz
057300010201     C                   move      vlrpoi        cmzdst
057400021113     C                   eval      cmzfld = 'FIVATWWR'
057500010201     C                   eval      cmzmbd = vlrhdl
057600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
057700021009     C***                if        prmfir = *blanks
057800021113     C                   eval      cmzfla = 'FIVAT00F'
057900021113     C                   eval      cmzmba = 'FIVAT00F'
058000021009     C***                else
058100021009     C***                eval      cmzfla = prmfir
058200021009     C***                eval      cmzmba = prmfir
058300021009     C***                endif
058400010201     C                   eval      cmznrr = *zeros
058500020305     C                   move      §ctrokvt      cmznrr
058600021018     C                   eval      cmzlba = vlrfl1
058700010201     C                   call(e)   'TIS711C'
058800010201     C                   parm                    dscmz
058900010201     C                   parm      *blanks       esito
059000010205     C                   if        %error
059100010205     C                             or cmzerr = '1'
059200010205     C                             or esito  = '1'
059300010205     C                   eval      wrkesito = '3'
059400010205     C                   else
059500010201     C*
059600021113     C* 2° invio FIVAB
059700010201     C                   reset                   dscmz
059800010201     C                   move      vlrpoi        cmzdst
059900010201     C                   eval      cmzfld = vlrfou
060000010201     C                   eval      cmzmbd = vlrhdl
060100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
060200021009     C***                if        prmfir = *blanks
060300021113     C                   eval      cmzfla = 'FIVAB00F'
060400021113     C                   eval      cmzmba = 'FIVAB00F'
060500021009     C***                else
060600021009     C***                eval      cmzfla = prmfir
060700021009     C***                eval      cmzmba = prmfir
060800021009     C***                endif
060900010201     C                   eval      cmznrr = *zeros
061000010201     C                   move      §ctrokvb      cmznrr
061100021018     C                   eval      cmzlba = vlrfl1
061200010201     C                   call(e)   'TIS711C'
061300010201     C                   parm                    dscmz
061400010201     C                   parm      *blanks       esito
061500010201     C                   if        %error
061600010201     C                             or cmzerr = '1'
061700010201     C                             or esito  = '1'
061800010201     C                   eval      wrkesito = '3'
061900010201     C                   endif
062000010205     C                   endif
062100990920     C*
062200000613     C                   ENDSR
062300000613     C***
062400070411
062500070411     C     *pssr         BEGSR
062600070411     C*
062700070411     C                   if        %open(tivin00r)
062800070411     C                   close     tivin00r
062900070411     C                   endif
063000070411     C                   if        %open(fivabwwr)
063100070411     C                   close     fivabwwr
063200070411     C                   endif
063300070411     C                   if        %open(fivatwwr)
063400070411     C                   close     fivatwwr
063500070411     C                   endif
063600070411     C*
063700070411     C* Effettuo la chiamata al CLLE preposto
063800070411     C                   call(e)   'TITVVTC'
063900070411     C                   parm                    parccm
064000070411     C                   parm                    parmbr
064100070411     C                   parm      '2'           paropz
064200070411     C*
064300070411     C                   eval      wrkesito = '2'
064400070411     C*
064500070411     C                   seton                                        LR
064600070411     C*
064700070411     C                   ENDSR     '*CANCL'
064800070411     C***
064900070411
065000990910
065100000613     C     *inzsr        BEGSR
065200990910     C*
065300990910     C     *entry        plist
065400990920     C                   parm                    tivlrds
065500990921     C                   parm      wrkesito      esito
065600000724     C                   parm                    prmlit
065700000710     C                   parm                    prmfir
065800000613     C*
065900000830     C* CALCOLA LA DATA CORRENTE
066000000830     C                   time                    wn14             14 0
066100000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
066200000830     C                   z-add     wn8           g08dat
066300000830     C                   z-add     *zeros        g08inv
066400000830     C                   movel     '0'           g08err
066500000830     C                   call      'XSRDA8'
066600000830     C                   parm                    wlbda8
066700000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
066800000830     C*
066900000613     C                   ENDSR
067000000613     C***
067100990908
067200060919     Otitvu8p   E            riepilogo         2
067300990915     O                                              'Upload via Internet'
067400990915     O                                           +1 'Traduzione TIVIN00R -'
067500021113     O                                           +1 'FIVABWWR/FIVATWWR'
067600010201     O                                           +1 'Report testate bolle'
067700990915     O          E            riepilogo   2
067800990915     O                       wrkdata
067900990915     O                       wrkora              +1
068000990915     O                       procname            +1
068100990915     O          E            riepilogo   2
068200990915     O                                              'Cliente..................:'
068300990915     O                       VABCCM        z     +1
068400990915     O          E            riepilogo   2
068500990920     O                                              'Riferimento Strategi.....:'
068600990920     O                       vlrhdl              +1
068700990915     O          E            riepilogo   2
068800990915     O                                              'Giusti...................:'
068900010201     O                       §CTROKVB      2   +  1
069000990915     O          E            riepilogo   2
069100990915     O                                              'Sbagliati e corretti.....:'
069200971022     O                       §CTRMO        2   +  1
069300990915     O          E            riepilogo   2
069400990915     O                                              'Sbagliati e scartati.....:'
069500971022     O                       §CTRNO        2   +  1
069600000613
069700060919     Otitvu8ps  E            testdett          2
069800000613     O                                              'Upload via Internet'
069900000613     O                                           +1 'Traduzione TIVIN00R -'
070000021113     O                                           +1 'FIVABWWR/FIVATWWR'
070100010201     O                                           +1 'Report testate bolle'
070200000616     O          E            testdett    3
070300000613     O                                           +2 'N° rec'
070400000613     O                                           +3 'Anteprima contenuto'
070500000616     O          E            rigadett    2
070600000613     O                       rrnum               +2
070700000621     O                       recko               +3
070800000616     O          E            findett     2
070900000613     O                       wrkdata
071000000613     O                       wrkora              +1
071100000613     O                       procname            +1
071200000616     O          E            findett     2
071300000613     O                                              'Cliente..................:'
071400000613     O                       VABCCM        z     +1
071500000616     O          E            findett     2
071600000613     O                                              'Riferimento Strategi.....:'
071700000613     O                       vlrhdl              +1
071800000616     O          E            findett     2
071900000613     O                                              'Giusti...................:'
072000010201     O                       §CTROKVB      2   +  1
072100000616     O          E            findett     2
072200000613     O                                              'Sbagliati e corretti.....:'
072300000613     O                       §CTRMO        2   +  1
072400000616     O          E            findett     2
072500000613     O                                              'Sbagliati e scartati.....:'
072600000613     O                       §CTRNO        2   +  1
072700000512** CMD - COMANDI CL
072800060919OVRPRTF FILE(TITVU8P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
072900060919OVRPRTF FILE(TITVU8PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
073000060919DLTOVR FILE(TITVU8P TITVU8PS) LVL(*)
073100000512
073200000512
