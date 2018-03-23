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
022900071210     C                   MOVEL     *blanks       wDEST            70
023000071210     C                   MOVEL     *blanks       wINDIR           45
023100071210     C                   MOVEL     *blanks       wCIVIC            6
023200071210     C                   MOVEL     *blanks       wNOTE            70
023300000801     C*
023400000801     C                   ENDSR
023500000801     C*----------------------------------------------------*
023600040910     C*  IMPOSTAZIONE CAMPI COSTANTI
023700000801     C*----------------------------------------------------*
023800000801     C     DEFCAM        BEGSR
023900000801     C*
024000021113     C                   CLEAR                   FIVAB000
024100040802     C                   CLEAR                   FIVAT000
024200020619     C* Imposto i valori di default...
024300100915     C                   Z-ADD     1141036       VABCCM
024400100915     C                   Z-ADD     1141036       VATCCM
024500100915     C                   Z-ADD     114           VABLNP
024600100915     C                   Z-ADD     114           VATLNP
024700040714     C                   Z-ADD     000           VABCTR
024800040823     C                   MOVEL     '1'           VABCBO
024900060206     C                   MOVEL     'D'           VABTSP
025000020619     C* ... e poi verifico se sono stati passati come parametri
025100020619     C                   IF        vlrppt > *blanks
025200040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
025300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
025400020619     C                   EXSR      CHKNUM
025500020619     C                   IF        PiInt=*on
025600020619     C                   Z-ADD     PiVal         VABCCM
025700020619     C                   Z-ADD     PiVal         VATCCM
025800020619     C                   ENDIF
025900040506     C                   ENDIF
026000040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
026100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
026200020619     C                   EXSR      CHKNUM
026300020619     C                   IF        PiInt=*on
026400020619     C                   Z-ADD     PiVal         VABLNP
026500020619     C                   Z-ADD     PiVal         VATLNP
026600040506     C                   ENDIF
026700020619     C                   ENDIF
026800040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
026900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
027000020619     C                   EXSR      CHKNUM
027100020619     C                   IF        PiInt=*on
027200020619     C                   Z-ADD     PiVal         VABCTR
027300040506     C                   ENDIF
027400020619     C                   ENDIF
027500020619     C                   ENDIF
027600000801     C*
027700000801     C                   ENDSR
027800000801     C*----------------------------------------------------*
027900040910     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB e FIVAT)
028000000801     C*----------------------------------------------------*
028100040910     C     IMPVABVAT     BEGSR
028200040910     C*
028300040910     C* Traduzione relativa ai tipi record del file del cliente
028400040910     C*
028500040910     C***
028600060919     C* ...tipo record 01
028700060919     C                   IF        %subst(vindta:1:2) = '01'
028800060919     C                   IF        wGiro = 1
028900060919     C*
029000060919     C* Considerazioni sul contenuto di campi precedentemente valorizzati
029100060919     C                   IF        FlgCAS <> '0'
029200060919     C                   IF        VABCBO = '1'
029300060919     C                   EVAL      VABCBO = '4'
029400060919     C                   ELSE
029500060919     C                   EVAL      VABCBO = '6'
029600060919     C                   ENDIF
029700060919     C                   ENDIF
029800101203     C*
029900101203     C                   MOVEL     'A'           VABTC1
030000101203     C*
030100101203     C* Solo x il codice 1141037 no appuntamento
030200101203     C                   IF        VABCCM = 1141037 and VABTC1 = 'A'
030300111212     C***                EVAL      VABTC1 = *blanks
030400101203     C                   ENDIF
030500060919     C*
030600060919     C* Eseguo routine finale x considerazioni specifiche su importi/divise
030700061025     C                   EXSR      CHKIMPDIV
030800060919     C*
030900060919     C* ...e in fine scarico il buffer d output
031000071029     C                   EXSR      wrivab
031100060919     C                   ELSE
031200060919     C                   Z-ADD     1             wGiro
031300060919     C                   ENDIF
031400060919     C*
031500040910     C* ......inizializzazioni iniziali e formati record file Bartolini
031600040910     C                   EXSR      INZVAR
031700040910     C                   EXSR      DEFCAM
031800040928     C*
031900040928     C                   Z-ADD     *zeros        errore            1 0
032000040928     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
032100040928     C                   MOVEL     datcor        VABAAS
032200040928     C                   MOVEL     datcor        VATAAS
032300040928     C                   MOVE      datcor        VABMGS
032400040928     C                   MOVE(P)   vlrpoi        VABFGS
032500040928     C                   MOVE(P)   vlrpoi        VATFGS
032600060919     C* ......VABNSP/VATNSP/VABRMN
032700060919     C                   EVAL      PiStr=%trim(%subst(vindta:4:9))
032800060919     C                   EXSR      CHKNUM
032900060919     C                   IF        PiInt=*on
033000060919     C                   Z-ADD     PiVal         VABNSP
033100060919     C                   Z-ADD     PiVal         VATNSP
033200060919     C                   Z-ADD     PiVal         VABRMN
033300060919     C                   ELSE
033400060919     C                   ADD       1             errore
033500060919     C                   EVAL      vinmsg = %trimr(vinmsg)
033600060919     C                             + ' ' + 'VABNSP/VATNSP/VABRMN'
033700060919     C                   ENDIF
033800060919     C* ......VABNAS
033900060919     C                   EVAL      VABNAS=%trim(%subst(vindta:19:20))
034000060919     C* ......VABRSD/VABRD2
034100061031     C                   EVAL      wDEST=%trim(%subst(vindta:53:35)) + ' '+
034200060919     C                                   %trim(%subst(vindta:213:35))
034300061018     C                   EVAL      VABRSD=%trim(%subst(wDEST:1:35))
034400061018     C                   EVAL      VABRD2=%trim(%subst(wDEST:36:35))
034500060919     C* ......VABNCL
034600060919     C                   EVAL      PiStr=%trim(%subst(vindta:41:4))
034700060919     C                   EXSR      CHKNUM
034800060919     C                   IF        PiInt=*on
034900060919     C                   ADD       PiVal         VABNCL
035000060919     C                   ELSE
035100060919     C                   ADD       1             errore
035200060919     C                   EVAL      vinmsg = %trimr(vinmsg)
035300060919     C                             + ' ' + 'VABNCL'
035400060919     C                   ENDIF
035500040910     C*
035600040910     C                   ENDIF
035700040910     C***
035800060919     C* ...tipo record 02
035900060919     C                   IF        %subst(vindta:1:2) = '02'
036000040910     C* ......VABIND
036100071113     C                   EVAL      wINDIR=%trim(%subst(vindta:3:45))
036200071113     C                   EVAL      wCIVIC=%trim(%subst(vindta:241:6))
036300040910     C* ......VABCAD
036400071022     C                   EVAL      VABCAD=%trim(%subst(vindta:48:6))
036500040910     C* ......VABLOD
036600071022     C                   EVAL      VABLOD=%trim(%subst(vindta:54:28))
036700060919     C* ......VABPRD
036800071022     C                   EVAL      VABPRD=%trim(%subst(vindta:82:2))
036900060919     C* ......VABNZD
037000071022     C                   EVAL      VABNZD=%trim(%subst(vindta:84:4))
037100060919     C                   IF        %trim(VABNZD) = 'IT'
037200060919     C                   EVAL      VABNZD=*blanks
037300061018     C                   ENDIF
037400060919     C* ......VABRMA
037500071022     C                   EVAL      VABRMA=%trim(%subst(vindta:188:8))
037600060919     C* ......VABPKB
037700071022     C                   EVAL      PiStr=%trim(%subst(vindta:202:7))
037800060919     C                   EXSR      CHKNUM
037900060919     C                   IF        PiNum=*on
038000060919     C                   EVAL(H)   VABPKB=PiVal/1000                            * gestisco 3 dec.
038100060919     C                   ELSE
038200060919     C                   ADD       1             errore
038300060919     C                   EVAL      vinmsg = %trimr(vinmsg)
038400060919     C                             + ' ' + 'VABPKB'
038500060919     C                   ENDIF
038600071115     C* ......VATNOT "A" - Referente destinatario (utilizzato xò x il 2° telefono)
038700080226     C                   EVAL      VATNOT=%trim(%subst(vindta:226:15))
038800060919     C                   IF        VATNOT <> *blanks
038900071115     C                   EVAL      VATTRC = 'A'
039000060919     C                   WRITE     FIVAT000
039100060919     C                   ENDIF
039200071115     C* ......VATNOT "B" - Telefono destinatario
039300080226     C                   EVAL      VATNOT=%trim(%subst(vindta:211:15))
039400071115     C                   IF        VATNOT <> *blanks
039500071115     C                   EVAL      VATTRC = 'B'
039600071115     C                   WRITE     FIVAT000
039700071115     C                   ENDIF
039800040910     C*
039900060919     C                   ENDIF
040000040910     C***
040100061018     C* ...tipo record 05
040200061018     C                   IF        %subst(vindta:1:2) = '05'
040300061018     C* ......VABCAS
040400060919     C                   IF        %subst(vindta:108:9) <> *zeros
040500040910     C                   EVAL      FlgCAS = '1'
040600060919     C                   EVAL      VABVCA='EUR'
040700060919     C                   EVAL      PiStr=%trim(%subst(vindta:108:9))
040800040910     C                   EXSR      CHKNUM
040900040910     C                   IF        PiNum=*on
041000061025     C                   EVAL(H)   VABCAS=PiVal/100                             * gestisco 2 dec.
041100040910     C                   ELSE
041200040910     C                   ADD       1             errore
041300040910     C                   EVAL      vinmsg = %trimr(vinmsg)
041400040910     C                             + ' ' + 'VABCAS'
041500040910     C                   ENDIF
041600061018     C                   ENDIF
041700040910     C*
041800060919     C                   ENDIF
041900040910     C***
042000061018     C* ...tipo record 06
042100061018     C                   IF        %subst(vindta:1:2) = '06'
042200060919     C* ......VABNOT/VABNT2
042300060919     C                   EVAL      wNOTE=%trim(%subst(vindta:3:99))
042400040910     C*
042500040910     C                   ENDIF
042600040910     C***
042700010202     C*
042800000801     C* Ebbene...
042900000801     C                   ADD       1             §CTRMO
043000010201     C                   IF        errore <> *zeros
043100000801     C                   ADD       1             §CTRNO
043200000801     C                   EVAL      recko = vindta
043300000801     C                   EXCEPT    rigadett
043400000801     C                   EVAL      vinflg = '2'
043500000801     C                   ELSE
043600010201     C                   ADD       1             §CTROKVB
043700000801     C                   ENDIF
043800000801     C*
043900000801     C                   ENDSR
044000071029
044100071029     C*----------------------------------------------------*
044200071029     C*  SCARICO BUFFER FILE TESTATE - FNVAB
044300071029     C*----------------------------------------------------*
044400071029     C     WRIVAB        BEGSR
044500071029     C*
044600071029     C* Verifico occupazione campo completo indirizzo
044700071113     C                   if        %len(%trim(wINDIR)) +
044800071113     C                             %len(%trim(wCIVIC)) +
044900071113     C                             1 > %size(VABIND)
045000071113     C                   eval      VABIND=%subst(%trim(wINDIR):1:
045100071113     C                                    %size(VABIND)-%len(%trim(wCIVIC))-1) +
045200071113     C                                    ' ' + %trim(wCIVIC)
045300071113     C                   eval      VABNOT=%subst(wNOTE:1:35)
045400071113     C                   eval      VABNT2=%subst(wNOTE:36:35)
045500071029     C                   else
045600071113     C                   eval      VABIND=%trim(wINDIR) + ' ' + %trim(wCIVIC)
045700071029     C                   eval      VABNOT=%subst(wNOTE:1:35)
045800071029     C                   eval      VABNT2=%subst(wNOTE:36:35)
045900071029     C                   endif
046000071029     C*
046100071029     C                   write     FIVAB000
046200071029     C*
046300071029     C                   ENDSR
046400010202     C*----------------------------------------------------*
046500021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
046600010202     C*----------------------------------------------------*
046700020305     C     PREVAT        BEGSR
046800010202     C*
046900021113     C* Compongo il nome del membro da dare al FIVATWWR
047000010202     C                   eval      parmbr = vlrhdl
047100010202     C                   movel     'M'           parmbr
047200060113     C                   eval      parccm = vlrksc
047300010202     C                   eval      paropz = '1'
047400010202     C* Effettuo la chiamata al CLLE preposto
047500040506     C                   call(e)   'TITVVTC'
047600010202     C                   parm                    parccm
047700010202     C                   parm                    parmbr
047800010202     C                   parm                    paropz
047900010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
048000010202     C                   if        %error
048100010202     C                   movel     '1'           chkcall
048200010202     C                   else
048300010202     C                   movel     '0'           chkcall
048400010202     C                   endif
048500010202     C*
048600010202     C                   ENDSR
048700000801     C*----------------------------------------------------*
048800000801     C*  CONTROLLO NUMERICITA' CAMPI
048900000801     C*----------------------------------------------------*
049000000801     C     CHKNUM        BEGSR
049100000801     C*
049200000801     C                   call(e)   'ISNUMERIC'
049300000801     C                   PARM                    PiStr            30
049400040714     C                   PARM      ','           PiDecChr          1
049500000801     C                   PARM      *ZEROS        PiVal            30 9
049600000801     C                   PARM      '0'           PiInt             1
049700000801     C                   PARM      '0'           PiNum             1
049800000801     C                   IF        %error
049900000801     C                   EVAL      PiInt=*off
050000000801     C                   ENDIF
050100000801     C*
050200000801     C                   ENDSR
050300000801     C***
050400000801
050500011113
050600011113     C*----------------------------------------------------*
050700011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
050800011113     C*----------------------------------------------------*
050900011113     C     CHKIMPDIV     BEGSR
051000011113     C*
051100011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
051200011113     C                   Z-ADD     *zeros        wrkDec            9 9
051300011113     C*
051400011113     C* Come prima cosa effettuo considerazioni sulla divisa
051500011113     C                   IF        vabIAS > *zeros
051600011113     C                   IF        vabVAS <> 'EUR'
051700011113     C                   EVAL      vabVAS =  'ITL'
051800011113     C                   ENDIF
051900011113     C                   ENDIF
052000011113     C*
052100011113     C                   IF        vabCAS > *zeros
052200011113     C                   IF        vabVCA <> 'EUR'
052300011113     C                   EVAL      vabVCA =  'ITL'
052400011113     C                   ENDIF
052500011113     C                   ENDIF
052600011113     C*
052700011113     C                   IF        vabVMD > *zeros
052800020305     C                   IF        vabVAD <> 'EUR'
052900011113     C                   EVAL      vabVAD =  'ITL'
053000011113     C                   ENDIF
053100011113     C                   ENDIF
053200011113     C*
053300011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
053400011113     C                   Z-ADD     vabIAS        wrkDec
053500011113     C                   IF        wrkDec > *zeros
053600011113     C                   IF        vabVAS = 'ITL'
053700011113     C                   EVAL      vabIAS = *zeros
053800011113     C                   ENDIF
053900011113     C                   ENDIF
054000011113     C*
054100011113     C* Stabilisco se il contrasegno ha decimali valorizzati
054200011113     C                   Z-ADD     vabCAS        wrkDec
054300011113     C                   IF        wrkDec > *zeros
054400011113     C                   IF        vabVCA = 'ITL'
054500011113     C                   EVAL      vabCAS = *zeros
054600011113     C                   ENDIF
054700011113     C                   ENDIF
054800011113     C*
054900011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
055000011113     C                   Z-ADD     vabVMD        wrkDec
055100011113     C                   IF        wrkDec > *zeros
055200011113     C                   IF        vabVAD = 'ITL'
055300011113     C                   EVAL      vabVMD = *zeros
055400011113     C                   ENDIF
055500011113     C                   ENDIF
055600011113     C*
055700011113     C                   ENDSR
055800011113     C***
055900011113
056000011113
056100000801
056200000801
056300990920      /TITLE Invio dei dati al punto operativo.
056400010202     C     invio         BEGSR
056500990920     C*
056600021113     C* 1° invio FIVAT
056700010201     C                   reset                   dscmz
056800010201     C                   move      vlrpoi        cmzdst
056900021113     C                   eval      cmzfld = 'FIVATWWR'
057000010201     C                   eval      cmzmbd = vlrhdl
057100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
057200021009     C***                if        prmfir = *blanks
057300021113     C                   eval      cmzfla = 'FIVAT00F'
057400021113     C                   eval      cmzmba = 'FIVAT00F'
057500021009     C***                else
057600021009     C***                eval      cmzfla = prmfir
057700021009     C***                eval      cmzmba = prmfir
057800021009     C***                endif
057900010201     C                   eval      cmznrr = *zeros
058000020305     C                   move      §ctrokvt      cmznrr
058100021018     C                   eval      cmzlba = vlrfl1
058200010201     C                   call(e)   'TIS711C'
058300010201     C                   parm                    dscmz
058400010201     C                   parm      *blanks       esito
058500010205     C                   if        %error
058600010205     C                             or cmzerr = '1'
058700010205     C                             or esito  = '1'
058800010205     C                   eval      wrkesito = '3'
058900010205     C                   else
059000010201     C*
059100021113     C* 2° invio FIVAB
059200010201     C                   reset                   dscmz
059300010201     C                   move      vlrpoi        cmzdst
059400010201     C                   eval      cmzfld = vlrfou
059500010201     C                   eval      cmzmbd = vlrhdl
059600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
059700021009     C***                if        prmfir = *blanks
059800021113     C                   eval      cmzfla = 'FIVAB00F'
059900021113     C                   eval      cmzmba = 'FIVAB00F'
060000021009     C***                else
060100021009     C***                eval      cmzfla = prmfir
060200021009     C***                eval      cmzmba = prmfir
060300021009     C***                endif
060400010201     C                   eval      cmznrr = *zeros
060500010201     C                   move      §ctrokvb      cmznrr
060600021018     C                   eval      cmzlba = vlrfl1
060700010201     C                   call(e)   'TIS711C'
060800010201     C                   parm                    dscmz
060900010201     C                   parm      *blanks       esito
061000010201     C                   if        %error
061100010201     C                             or cmzerr = '1'
061200010201     C                             or esito  = '1'
061300010201     C                   eval      wrkesito = '3'
061400010201     C                   endif
061500010205     C                   endif
061600990920     C*
061700000613     C                   ENDSR
061800000613     C***
061900070411
062000070411     C     *pssr         BEGSR
062100070411     C*
062200070411     C                   if        %open(tivin00r)
062300070411     C                   close     tivin00r
062400070411     C                   endif
062500070411     C                   if        %open(fivabwwr)
062600070411     C                   close     fivabwwr
062700070411     C                   endif
062800070411     C                   if        %open(fivatwwr)
062900070411     C                   close     fivatwwr
063000070411     C                   endif
063100070411     C*
063200070411     C* Effettuo la chiamata al CLLE preposto
063300070411     C                   call(e)   'TITVVTC'
063400070411     C                   parm                    parccm
063500070411     C                   parm                    parmbr
063600070411     C                   parm      '2'           paropz
063700070411     C*
063800070411     C                   eval      wrkesito = '2'
063900070411     C*
064000070411     C                   seton                                        LR
064100070411     C*
064200070411     C                   ENDSR     '*CANCL'
064300070411     C***
064400070411
064500990910
064600000613     C     *inzsr        BEGSR
064700990910     C*
064800990910     C     *entry        plist
064900990920     C                   parm                    tivlrds
065000990921     C                   parm      wrkesito      esito
065100000724     C                   parm                    prmlit
065200000710     C                   parm                    prmfir
065300000613     C*
065400000830     C* CALCOLA LA DATA CORRENTE
065500000830     C                   time                    wn14             14 0
065600000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
065700000830     C                   z-add     wn8           g08dat
065800000830     C                   z-add     *zeros        g08inv
065900000830     C                   movel     '0'           g08err
066000000830     C                   call      'XSRDA8'
066100000830     C                   parm                    wlbda8
066200000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
066300000830     C*
066400000613     C                   ENDSR
066500000613     C***
066600990908
066700060919     Otitvu8p   E            riepilogo         2
066800990915     O                                              'Upload via Internet'
066900990915     O                                           +1 'Traduzione TIVIN00R -'
067000021113     O                                           +1 'FIVABWWR/FIVATWWR'
067100010201     O                                           +1 'Report testate bolle'
067200990915     O          E            riepilogo   2
067300990915     O                       wrkdata
067400990915     O                       wrkora              +1
067500990915     O                       procname            +1
067600990915     O          E            riepilogo   2
067700990915     O                                              'Cliente..................:'
067800990915     O                       VABCCM        z     +1
067900990915     O          E            riepilogo   2
068000990920     O                                              'Riferimento Strategi.....:'
068100990920     O                       vlrhdl              +1
068200990915     O          E            riepilogo   2
068300990915     O                                              'Giusti...................:'
068400010201     O                       §CTROKVB      2   +  1
068500990915     O          E            riepilogo   2
068600990915     O                                              'Sbagliati e corretti.....:'
068700971022     O                       §CTRMO        2   +  1
068800990915     O          E            riepilogo   2
068900990915     O                                              'Sbagliati e scartati.....:'
069000971022     O                       §CTRNO        2   +  1
069100000613
069200060919     Otitvu8ps  E            testdett          2
069300000613     O                                              'Upload via Internet'
069400000613     O                                           +1 'Traduzione TIVIN00R -'
069500021113     O                                           +1 'FIVABWWR/FIVATWWR'
069600010201     O                                           +1 'Report testate bolle'
069700000616     O          E            testdett    3
069800000613     O                                           +2 'N° rec'
069900000613     O                                           +3 'Anteprima contenuto'
070000000616     O          E            rigadett    2
070100000613     O                       rrnum               +2
070200000621     O                       recko               +3
070300000616     O          E            findett     2
070400000613     O                       wrkdata
070500000613     O                       wrkora              +1
070600000613     O                       procname            +1
070700000616     O          E            findett     2
070800000613     O                                              'Cliente..................:'
070900000613     O                       VABCCM        z     +1
071000000616     O          E            findett     2
071100000613     O                                              'Riferimento Strategi.....:'
071200000613     O                       vlrhdl              +1
071300000616     O          E            findett     2
071400000613     O                                              'Giusti...................:'
071500010201     O                       §CTROKVB      2   +  1
071600000616     O          E            findett     2
071700000613     O                                              'Sbagliati e corretti.....:'
071800000613     O                       §CTRMO        2   +  1
071900000616     O          E            findett     2
072000000613     O                                              'Sbagliati e scartati.....:'
072100000613     O                       §CTRNO        2   +  1
072200000512** CMD - COMANDI CL
072300060919OVRPRTF FILE(TITVU8P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
072400060919OVRPRTF FILE(TITVU8PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
072500060919DLTOVR FILE(TITVU8P TITVU8PS) LVL(*)
072600000512
072700000512
