000100120611      /TITLE Upload via Internet: traduzione in FIVAB/FIVAT x GRUPPO IRCA (ZOPPAS)
000200040706     H*                           Cod. 0590376 / 0590499 / 0590422 / 0590502 / 0590098 / 0590132
000300060523     H*                                0592665 / 0592966 / 0592967 / ...
000400990908     H dftactgrp(*yes)
000500000313     F*
000600990910     Ftivin00r  uF   E             DISK    usropn
000700021014     FFIVABwwr  O    E             DISK    usropn
000800120611     FFIVATwwr  O    E             DISK    usropn
000900000313     D*
001000010330     D*----------------------------------------------------
001100010330     D* DICHIARAZIOINE VARIABILI DI WRK
001200010330     D*----------------------------------------------------
001300010330     D dscmz         e ds                  inz
001400010330     D psds           sds
001500010330     D  procname         *PROC
001600010330     D tivlrds       e ds                  extname(tivlr00f)
001700010330     D esito           s              1
001800010330     D prmlit          s             10
001900010330     D prmfir          s             10
002000010330     D wrkesito        s                   like(esito)
002100010330     D rrnum           s              6  0 INZ(*zeros)
002200120611     D parccm          s              8    INZ(*blanks)
002300120611     D parmbr          s             10    INZ(*blanks)
002400120611     D paropz          s              1    INZ(*blanks)
002500120611     D chkcall         s              1    INZ(*blanks)
002600010330
002700010330
002800010330
002900000913     C                   reset                   rrnum
003000990921     C                   reset                   esito
003100990921     C                   reset                   wrkesito
003200010601     C*
003300010601     C                   exsr      opeini
003400010605     C                   exsr      rwvab
003500120611     C*
003600120611     C* Effettuo la chiamata al CLLE preposto
003700120611     C                   call(e)   'TITVVTC'
003800120611     C                   parm                    parccm
003900120611     C                   parm                    parmbr
004000120611     C                   parm      '2'           paropz
004100010601     C*
004200010601     C                   seton                                        lr
004300010601
004400010601
004500010601
004600010601
004700010601     C*--------------------------------------------------------
004800010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
004900010601     C*--------------------------------------------------------
005000010601     C     PREELA        BEGSR
005100010601     C*
005200010601     C* SE OCCORRE SPEDIRE IN FILIALE
005300010601     C                   if        invfil <> *zeros and
005400010601     C                             flgGiro = '0'
005500010601     C*
005600010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
005700010601     C                   eval      flgGiro = '1'
005800010601     C*
005900010601     C                   endif
006000010601     C*
006100010601     C                   ENDSR
006200010601     C***
006300010601
006400010601
006500010601
006600010601     C*--------------------------------------------------------
006700010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
006800010601     C*--------------------------------------------------------
006900010601     C     ENDELA        BEGSR
007000120611     C*
007100000616     C*
007200010601     C                   ENDSR
007300010601     C***
007400000613
007500010601
007600010601
007700010330     C*--------------------------------------------------------
007800040301     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
007900010330     C*--------------------------------------------------------
008000010605     C     RWVAB         BEGSR
008100010330     C*
008200010330     C                   if        not %open(tivin00r)
008300010330     C                   open      tivin00r
008400010330     C                   endif
008500021014     C                   if        not %open(fivabwwr)
008600021014     C                   open      fivabwwr
008700010330     C                   endif
008800120611     C*
008900120611     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
009000120611     C                   exsr      prevat
009100120611     C*
009200120611     C                   if        chkcall = '0'
009300120611     C*
009400120611     C                   if        not %open(fivatwwr)
009500120611     C                   open      fivatwwr
009600120611     C                   endif
009700010330     C*
009800120611     C                   clear                   §CTROKVB          7 0
009900120611     C                   clear                   §CTROKVT          7 0
010000010604     C                   clear                   §CTRMO
010100010604     C                   clear                   §CTRNO
010200120615     C*
010300120615     C                   exsr      inzvar
010400120615     C                   exsr      defcam
010500010330     C*
010600010330     C                   DO        *HIVAL
010700010330     C*
010800010330     C                   READ      tivin00r                               70
010900010618     C*
011000010618     C* Dopo ogni lettura verifico se ci sono stati record OK
011100010618     C                   if        vinflg = '1'
011200010618     C                   eval      flgOk = '1'
011300010618     C                   endif
011400010618     C*
011500010330     C                   if        vindta > *blanks
011600010330     C                   add       1             rrnum
011700010330     C*
011800010601     C                   if        *in70 = *off and
011900010330     C                             (vinflg = *blanks
012000010330     C                              or vinflg = '0'
012100010330     C                              or vinflg = '2')
012200010330     C*
012300010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
012400010711     C                   if        vinflg = *blanks or vinflg = '0'
012500010711     C                   clear                   vinmsg
012600010711     C                   endif
012700010601     C*
012800120611     C                   if        %subst(vindta:33:2)  = '20' and
012900120611     C                             %subst(vindta:106:1) = 'S'
013000120611     C*
013100120611     C* Tipo record dettaglio
013200120611     C                   exsr      impvat
013300120611     C*
013400120611     C* Scarico il buffer del FIVAT
013500120611     C  N31              WRITE     FIVAT000
013600120611     C  N31              add       1             §CTROKVT
013700120611     C*
013800120611     C                   else
013900120611     C*
014000120611     C* Tipo record testata
014100010605     C                   exsr      impvab
014200010601     C*
014300010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
014400010605     C                   eval      depfil = VABLNP
014500010601     C                   exsr      repfil
014600010601     C                   if        depfil = invfil
014700021025     C                   if        vlrpoi = 999
014800021025     C                   MOVE(P)   invfil        VABFGS
014900021025     C                   else
015000021025     C                   MOVE(P)   vlrpoi        VABFGS
015100021025     C                   endif
015200010601     C*
015300010601     C                   exsr      PREELA
015400010601     C*
015500010604     C* Ebbene...
015600010604     C*
015700010604     C   32              ADD       1             §CTRMO            7 0
015800010604     C   31              ADD       1             §CTRNO            7 0
015900020205     C*
016000040301     C* Scarico il buffer del FIVAB
016100021014     C  N31              WRITE     FIVAB000
016200120615     C  N31              add       1             §CTROKVB
016300120611     C                   endif
016400010604     C*
016500020722     C                   endif
016600020722     C*
016700010604     C                   if        *in31 = *off and
016800010604     C                             *in32 = *off
016900010604     C                   eval      vinflg = '1'
017000010604     C                   else
017100010604     C                   eval      vinflg = '2'
017200010604     C                   endif
017300010604     C                   endif
017400010604     C*
017500010330     C                   else
017600010330     C                   eval      vinflg = '1'
017700010330     C                   endif
017800010601     C*
017900010601     C  N70              update    tivin000
018000010330     C*
018100010330     C  N70              ENDdo
018200120611     C*
018300120611     C                   endif                                                  (endif - chkcall)
018400010601     C*
018500010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
018600010601     C                   if        cntNonEl = *zeros or
018700010601     C                             flgMulti = '0'
018800010330     C* Se non ci sono record con errori ...
018900010601     C                   if        §ctrno = 0 and
019000010604     C                             §ctrmo = 0 and
019100010601     C                             flgStato <> '2'
019200010330     C* ... restituisco esito OK.
019300010330     C                   eval      wrkesito = '0'
019400010330     C                   else
019500120611     C                   if        §ctrokvb > 0
019600010330     C                   eval      wrkesito = '1'
019700010330     C                   else
019800010615     C                   if        flgOk = '0'
019900010615     C                   eval      wrkesito = '2'
020000010615     C                   else
020100010615     C                   eval      wrkesito = '6'
020200010615     C                   endif
020300010330     C                   endif
020400010330     C                   endif
020500010601     C                   else
020600010601     C                   eval      wrkesito = '9'
020700010601     C                   endif
020800010330     C*
020900010330     C                   if        %open(tivin00r)
021000010330     C                   close     tivin00r
021100010330     C                   endif
021200021014     C                   if        %open(fivabwwr)
021300021014     C                   close     fivabwwr
021400010330     C                   endif
021500120611     C                   if        %open(fivatwwr)
021600120611     C                   close     fivatwwr
021700120611     C                   endif
021800010601     C*
021900010601     C                   if        vlrpoi <> 999
022000010601     C                   eval      invfil = vlrpoi
022100010601     C                   endif
022200010330     C*
022300120611     C                   if        §ctrokvb > 0
022400010601     C                             and invfil > *zeros
022500010330     C                   exsr      invio
022600010330     C                   endif
022700010601     C*
022800010618     C                   if        flgGiro = '1'
022900010601     C                   exsr      endela
023000010618     C                   endif
023100010330     C*
023200010330     C                   ENDSR
023300010330     C***
023400120611
023500120611
023600120611
023700120611     C*----------------------------------------------------*
023800120611     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
023900120611     C*----------------------------------------------------*
024000120611     C     PREVAT        BEGSR
024100120611     C*
024200120611     C* Compongo il nome del membro da dare al FIVATWWR
024300120611     C                   eval      parmbr = vlrhdl
024400120611     C                   movel     'M'           parmbr
024500120611     C                   eval      parccm = vlrksc
024600120611     C                   eval      paropz = '1'
024700120611     C* Effettuo la chiamata al CLLE preposto
024800120611     C                   call(e)   'TITVVTC'
024900120611     C                   parm                    parccm
025000120611     C                   parm                    parmbr
025100120611     C                   parm                    paropz
025200120611     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
025300120611     C                   if        %error
025400120611     C                   movel     '1'           chkcall
025500120611     C                   else
025600120611     C                   movel     '0'           chkcall
025700120611     C                   endif
025800120611     C*
025900120611     C                   ENDSR
026000010601
026100010601
026200010601
026300010330     C*----------------------------------------------------*
026400020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
026500010330     C*----------------------------------------------------*
026600010330     C     INZVAR        BEGSR
026700010330     C*
026800020204     C                   Z-ADD     *zeros        Num5_0            5 0
026900020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
027000020322     C                   MOVEL     '0'           FlgCAS            1
027100010330     C*
027200010330     C                   ENDSR
027300010330     C*----------------------------------------------------*
027400020722     C*  IMPOSTAZIONE CAMPI COSTANTI
027500010330     C*----------------------------------------------------*
027600010330     C     DEFCAM        BEGSR
027700010330     C*
027800020204     C* Inizializzo il buffer del record da scrivere
027900021014     C                   CLEAR                   FIVAB000
028000120611     C                   CLEAR                   FIVAT000
028100020204     C* Imposto i valori di default...
028200040706     C                   EVAL      VABLNP = 059
028300040706     C                   EVAL      VABCTR = 000
028400040706     C                   EVAL      VABTSP = 'C'
028500040706     C                   EVAL      VABCBO = '1'
028600120611     C                   EVAL      VABCTM = '7Q'
028700040707     C                   MOVEL     'I'           wIOE              1
028800060523     C                   MOVEL     'RICA'        wDIV              4
028900060523     C                   MOVEL     'F'           wPROD             1
029000020204     C*
029100010330     C                   ENDSR
029200010607     C*----------------------------------------------------*
029300021014     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
029400010607     C*----------------------------------------------------*
029500010607     C     IMPVAB        BEGSR
029600010607     C*
029700010607     C                   SETOFF                                       3132
029800010607     C*
029900010607     C* Reperimento campi ALFA
030000010607     C*
030100010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
030200040706     C                   EVAL      VABRSD=%trim(%subst(vindta:225:35))
030300020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
030400020117     C     '@':'A'       XLATE     VABRSD        VABRSD
030500020117     C* ==
030600040706     C                   EVAL      VABIND=%trim(%subst(vindta:260:35))
030700040706     C                   EVAL      VABLOD=%trim(%subst(vindta:300:25))
030800040706     C                   EVAL      VABPRD=%trim(%subst(vindta:325:2))
030900040707     C***                EVAL      VABNZD=%trim(%subst(vindta:327:4))
031000040708     C                   EVAL      VABRMA=%trim(%subst(vindta:7:16))
031100040706     C                   EVAL      VABNOT=%trim(%subst(vindta:388:35))
031200130212     C                   EVAL      VABNT2=%trim(%subst(vindta:388+35:33))
031300040706     C                   IF        %trim(%subst(vindta:347:10)) = 'VE019'
031400040706     C                   EVAL      VABTSP='C'
031500040706     C                   ENDIF
031600040706     C                   IF        %trim(%subst(vindta:347:10)) = 'VE094'
031700040706     C                   EVAL      VABTSP='E'
031800040706     C                   ENDIF
031900040706     C                   IF        %trim(%subst(vindta:382:4)) = 'DDU'
032000040706     C                   EVAL      VABCBO='1'
032100040706     C                   ENDIF
032200040706     C                   IF        %trim(%subst(vindta:382:4)) = 'EXW'
032300040706     C                   EVAL      VABCBO='2'
032400040706     C                   ENDIF
032500040706     C                   IF        %trim(%subst(vindta:386:2)) = 'SI'
032600040706     C                   EVAL      FlgCAS='1'
032700040706     C                   ENDIF
032800040707     C*
032900040707     C* Considerazioni particolari su spedizione ITALIA o ESTERO
033000060523     C                   IF        %subst(vindta:23:3) = 'DDT'
033100060523     C                   MOVEL     'I'           wIOE
033200060523     C                   ELSE
033300040707     C                   MOVEL     'E'           wIOE
033400040707     C                   ENDIF
033500060523     C*
033600060523     C* Considerazioni particolari su prodotto FINITO o c/to LAVORO
033700060523     C                   IF        wIOE = 'I'
033800060523     C                   IF        %subst(vindta:28:1) = '0' OR
033900060523     C                             %subst(vindta:28:1) = '2' OR
034000060523     C                             %subst(vindta:28:1) = '3'
034100060523     C                   MOVEL     'F'           wPROD
034200060523     C                   ELSE
034300060523     C                   IF        %subst(vindta:28:1) = '6' OR
034400060523     C                             %subst(vindta:28:1) = '7' OR
034500060523     C                             %subst(vindta:28:1) = '8'
034600060523     C                   MOVEL     'L'           wPROD
034700060523     C                   ENDIF
034800060523     C                   ENDIF
034900060523     C                   ENDIF
035000060523     C*
035100060523     C* Considerazioni particolari sulla DIVISIONE
035200060523     C                   IF        %subst(vindta:1:6) = '000021'
035300060523     C*    */ Divisione RICA */
035400060531     C                   MOVEL(P)  'RICA'        wDIV
035500060523     C                   ELSE
035600060523     C                   IF        %subst(vindta:1:6) = '000022'
035700060523     C*    */ Divisione SEV */
035800060531     C                   MOVEL(P)  'SEV'         wDIV
035900060523     C                   ELSE
036000060523     C                   IF        %subst(vindta:1:6) = '000023'
036100060523     C*    */ Divisione IRCA */
036200060531     C                   MOVEL(P)  'IRCA'        wDIV
036300060523     C                   ENDIF
036400060523     C                   ENDIF
036500060523     C                   ENDIF
036600060523     C*
036700060523     C* Considerazioni finali sull'attribuzione del codice cliente mittente
036800060523     C                   IF        wDIV = 'IRCA'
036900060523     C                   IF        wIOE = 'I' AND wPROD = 'F'
037000060523     C                   EVAL      VABCCM = 0590098
037100060523     C                   ENDIF
037200060523     C                   IF        wIOE = 'I' AND wPROD = 'L'
037300060523     C                   EVAL      VABCCM = 0592965
037400060523     C                   ENDIF
037500060523     C                   IF        wIOE = 'E'
037600060523     C                   EVAL      VABCCM = 0590132
037700060523     C                   ENDIF
037800060523     C                   ENDIF
037900060523     C*
038000060523     C                   IF        wDIV = 'RICA'
038100060523     C                   IF        wIOE = 'I' AND wPROD = 'F'
038200060523     C                   EVAL      VABCCM = 0590376
038300060523     C                   ENDIF
038400060523     C                   IF        wIOE = 'I' AND wPROD = 'L'
038500060523     C                   EVAL      VABCCM = 0592966
038600060523     C                   ENDIF
038700060523     C                   IF        wIOE = 'E'
038800060523     C                   EVAL      VABCCM = 0590499
038900060523     C                   ENDIF
039000060523     C                   ENDIF
039100060523     C*
039200060523     C                   IF        wDIV = 'SEV'
039300060523     C                   IF        wIOE = 'I' AND wPROD = 'F'
039400060523     C                   EVAL      VABCCM = 0590422
039500060523     C                   ENDIF
039600060523     C                   IF        wIOE = 'I' AND wPROD = 'L'
039700060523     C                   EVAL      VABCCM = 0592967
039800060523     C                   ENDIF
039900060523     C                   IF        wIOE = 'E'
040000060523     C                   EVAL      VABCCM = 0590502
040100060523     C                   ENDIF
040200060523     C                   ENDIF
040300040707     C*
040400040707     C* Considerazioni particolari sulla tariffa ITALIA o ESTERO
040500040707     C                   IF        wIOE = 'E'
040600040707     C                   EVAL      VABCTR = 099
040700040707     C                   ENDIF
040800060523     C*
040900040706     C                   IF        VABCCM = *zeros
041000040706     C                   SETON                                        31
041100040706     C                   ENDIF
041200010607     C*
041300040706     C* Reperimento campi NUMERICI
041400020722     C                   MOVEL     DATCOR        VABAAS
041500020722     C                   MOVE      DATCOR        VABMGS
041600040420     C* NSP/RMN
041700040707     C                   Z-ADD     *zeros        wPOS              2 0
041800040707     C                   EVAL      wPOS=%scan('/':%subst(vindta:23:10))
041900040707     C                   EVAL      PiStr=%trim(%subst(vindta:23+wPOS:10-wPOS))
042000020304     C                   EXSR      CHKNUM
042100020304     C                   IF        PiInt=*on
042200040301     C                   Z-ADD     PiVal         VABNSP
042300040301     C                   Z-ADD     PiVal         VABRMN
042400020304     C                   ELSE
042500020722     C                   SETON                                        31
042600020304     C                   Z-ADD     *zeros        VABNSP
042700040301     C                   Z-ADD     *zeros        VABRMN
042800020304     C                   EVAL      vinmsg = %trimr(vinmsg)
042900040301     C                             + ' ' + 'VABNSP VABRMN'
043000020304     C                   ENDIF
043100040420     C* CAD
043200040706     C                   EVAL      PiStr=%trim(%subst(vindta:295:5))
043300010607     C                   EXSR      CHKNUM
043400010607     C                   IF        PiInt=*on
043500020204     C                   Z-ADD     PiVal         Num5_0
043600020204     C                   MOVEL(p)  Num5_0        VABCAD
043700010607     C                   ELSE
043800010607     C                   SETON                                        32
043900020204     C                   EVAL      VABCAD = *zeros
044000010607     C                   EVAL      vinmsg = %trimr(vinmsg)
044100020204     C                             + ' ' + 'VABCAD'
044200010607     C                   ENDIF
044300040420     C* NCL
044400040706     C                   EVAL      PiStr=%trim(%subst(vindta:357:5))
044500010607     C                   EXSR      CHKNUM
044600010607     C                   IF        PiInt=*on
044700010607     C                   Z-ADD     PiVal         VABNCL
044800010607     C                   ELSE
044900010607     C                   SETON                                        32
045000010607     C                   Z-ADD     *zeros        VABNCL
045100010607     C                   EVAL      vinmsg = %trimr(vinmsg)
045200010607     C                             + ' ' + 'VABNCL'
045300010607     C                   ENDIF
045400040420     C* PKB
045500040706     C                   EVAL      PiStr=%trim(%subst(vindta:362:10))
045600010607     C                   EXSR      CHKNUM
045700010607     C                   IF        PiNum=*on
045800040420     C                   Z-ADD     PiVal         VABPKB
045900040608     C                   EVAL(H)   VABPKB = PiVal/1000                          * gestisco 3 dec.
046000010607     C                   ELSE
046100010607     C                   SETON                                        32
046200010607     C                   Z-ADD     *zeros        VABPKB
046300010607     C                   EVAL      vinmsg = %trimr(vinmsg)
046400010607     C                             + ' ' + 'VABPKB'
046500010607     C                   ENDIF
046600040706     C* VLB
046700040706     C                   EVAL      PiStr=%trim(%subst(vindta:372:10))
046800040706     C                   EXSR      CHKNUM
046900040706     C                   IF        PiNum=*on
047000040706     C                   Z-ADD     PiVal         VABVLB
047100040706     C                   EVAL(H)   VABVLB = PiVal/1000                          * gestisco 3 dec.
047200040706     C                   ELSE
047300040706     C                   SETON                                        32
047400040706     C                   Z-ADD     *zeros        VABVLB
047500040706     C                   EVAL      vinmsg = %trimr(vinmsg)
047600040706     C                             + ' ' + 'VABVLB'
047700040706     C                   ENDIF
047800130212     C* CAS
047900130212     C                   IF        %subst(vindta:588:10) <> *blanks
048000130212     C                   EVAL      VABTIC = 'BM'
048100130228     C                   EVAL      vabVCA = 'EUR'
048200130212     C                   EVAL      PiStr=%trim(%subst(vindta:588:10))
048300130212     C                   EXSR      CHKNUM
048400130212     C                   IF        PiNum=*on
048500130212     C                   Z-ADD     PiVal         VABCAS
048600130212     C                   EVAL(H)   VABCAS = PiVal/100                           * gestisco 2 dec.
048700130212     C                   ELSE
048800130212     C                   SETON                                        32
048900130212     C                   Z-ADD     *zeros        VABCAS
049000130212     C                   EVAL      vinmsg = %trimr(vinmsg)
049100130212     C                             + ' ' + 'VABCAS'
049200130212     C                   ENDIF
049300130212     C                   ENDIF
049400020322     C*
049500020322     C* Considerazioni finali su CBO/CAS
049600020322     C                   IF        FlgCAS = '1'
049700040706     C                   IF        VABCBO = '1'
049800020322     C                   EVAL      VABCBO = '4'
049900040706     C                   ENDIF
050000040706     C                   IF        VABCBO = '2'
050100040706     C                   EVAL      VABCBO = '6'
050200020322     C                   ENDIF
050300040706     C                   ENDIF
050400020204     C*
050500020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
050600020204     C                   EXSR      CHKIMPDIV
050700020204     C*
050800010607     C                   ENDSR
050900010607     C*----------------------------------------------------*
051000120611
051100120611
051200120611     C*----------------------------------------------------*
051300120611     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
051400120611     C*----------------------------------------------------*
051500120611     C     IMPVAT        BEGSR
051600120611     C*
051700120611     C                   SETOFF                                       31
051800120611     C*
051900120627     C                   EVAL      VATFGS = VABFGS
052000120611     C                   EVAL      VATAAS = VABAAS
052100120611     C                   EVAL      VATLNP = VABLNP
052200120611     C                   EVAL      VATNRS = VABNRS
052300120611     C                   EVAL      VATNSP = VABNSP
052400120611     C                   EVAL      VATCCM = VABCCM
052500120611     C*
052600120611     C                   EVAL      VATTRC = 'E'
052700120611     C                   EVAL      VATNOT = %trim(%subst(vindta:45:11))
052800120611     C*
052900120627     C                   IF        VATAAS = *zeros  AND
053000120627     C                             VATLNP = *zeros  AND
053100120627     C                             VATNRS = *zeros  AND
053200120627     C                             VATNSP = *zeros  AND
053300120627     C                             VATCCM = *zeros  AND
053400120611     C                             VATNOT = *blanks
053500120611     C                   SETON                                        31
053600120611     C                   ENDIF
053700120611     C*
053800120611     C                   ENDSR
053900120611     C*----------------------------------------------------*
054000010601
054100020204
054200020204     C*----------------------------------------------------*
054300020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
054400020204     C*----------------------------------------------------*
054500020204     C     CHKIMPDIV     BEGSR
054600020204     C*
054700020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
054800020204     C                   Z-ADD     *zeros        wrkDec            9 9
054900020204     C*
055000020204     C* Come prima cosa effettuo considerazioni sulla divisa
055100020204     C                   IF        vabIAS > *zeros
055200020204     C                   IF        vabVAS <> 'EUR'
055300020204     C                   EVAL      vabVAS =  'ITL'
055400020204     C                   ENDIF
055500020204     C                   ENDIF
055600020204     C*
055700020204     C                   IF        vabCAS > *zeros
055800020204     C                   IF        vabVCA <> 'EUR'
055900020204     C                   EVAL      vabVCA =  'ITL'
056000020204     C                   ENDIF
056100020204     C                   ENDIF
056200020204     C*
056300020204     C                   IF        vabVMD > *zeros
056400020321     C                   IF        vabVAD <> 'EUR'
056500020204     C                   EVAL      vabVAD =  'ITL'
056600020204     C                   ENDIF
056700020204     C                   ENDIF
056800020204     C*
056900020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
057000020204     C                   Z-ADD     vabIAS        wrkDec
057100020204     C                   IF        wrkDec > *zeros
057200020204     C                   IF        vabVAS = 'ITL'
057300020204     C                   EVAL      vabIAS = *zeros
057400020204     C                   ENDIF
057500020204     C                   ENDIF
057600020204     C*
057700020204     C* Stabilisco se il contrasegno ha decimali valorizzati
057800020204     C                   Z-ADD     vabCAS        wrkDec
057900020204     C                   IF        wrkDec > *zeros
058000020204     C                   IF        vabVCA = 'ITL'
058100020204     C                   EVAL      vabCAS = *zeros
058200020204     C                   ENDIF
058300020204     C                   ENDIF
058400020204     C*
058500020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
058600020204     C                   Z-ADD     vabVMD        wrkDec
058700020204     C                   IF        wrkDec > *zeros
058800020204     C                   IF        vabVAD = 'ITL'
058900020204     C                   EVAL      vabVMD = *zeros
059000020204     C                   ENDIF
059100020204     C                   ENDIF
059200020204     C*
059300020204     C                   ENDSR
059400020204     C***
059500020204
059600010330
059700010330
059800010330     C*----------------------------------------------------*
059900010330     C*  CONTROLLO NUMERICITA' CAMPI
060000010330     C*----------------------------------------------------*
060100010330     C     CHKNUM        BEGSR
060200010330     C*
060300010606     C                   IF        PiDecChr = *blanks
060400010606     C                   EVAL      PiDecChr = ','
060500010606     C                   ENDIF
060600010606     C*
060700010606     C                   CALL(e)   'ISNUMERIC'
060800010330     C                   PARM                    PiStr            30
060900010606     C                   PARM                    PiDecChr          1
061000010330     C                   PARM      *ZEROS        PiVal            30 9
061100010330     C                   PARM      '0'           PiInt             1
061200010330     C                   PARM      '0'           PiNum             1
061300010330     C                   IF        %error
061400010606     C                   EVAL      PiNum=*off
061500010330     C                   ENDIF
061600010330     C*
061700010330     C                   ENDSR
061800010330     C***
061900010330
062000010601
062100010601
062200010601
062300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
062400010601     C     repfil        BEGSR
062500010601     C*
062600010601     C                   if        invfil = *zeros and
062700010601     C                             depfil > *zeros and
062800010629     C                             (vinflg = *blanks or
062900010629     C                              vinflg = *zeros)
063000010601     C
063100010601     C                   eval      invfil = depfil
063200010601     C                   endif
063300010601     C*
063400010601     C                   if        depfil <> invfil and
063500010601     C                             invfil > *zeros
063600010601     C                   eval      flgMulti = '1'
063700010601     C                   if        vinflg = *blanks
063800010601     C                   add       1             cntNonEl
063900010601     C                   endif
064000010601     C                   endif
064100010601     C*
064200010601     C                   if        vinflg = '2'
064300010601     C                   eval      flgStato = '2'
064400010601     C                   endif
064500010601     C*
064600010601     C                   ENDSR
064700010601     C***
064800120611
064900120611
065000120611
065100120611      /TITLE Invio dei dati al punto operativo.
065200120611     C     invio         BEGSR
065300120611     C*
065400120611     C* 1° invio FIVAT
065500120611     C                   reset                   dscmz
065600120611     C                   move      vlrpoi        cmzdst
065700120611     C                   eval      cmzfld = 'FIVATWWR'
065800120611     C                   eval      cmzmbd = vlrhdl
065900120611     C                   eval      %subst(cmzmbd:1:1) = 'M'
066000120611     C***                if        prmfir = *blanks
066100120611     C                   eval      cmzfla = 'FIVAT00F'
066200120611     C                   eval      cmzmba = 'FIVAT00F'
066300120611     C***                else
066400120611     C***                eval      cmzfla = prmfir
066500120611     C***                eval      cmzmba = prmfir
066600120611     C***                endif
066700120611     C                   eval      cmznrr = *zeros
066800120611     C                   move      §ctrokvt      cmznrr
066900120611     C                   eval      cmzlba = vlrfl1
067000120611     C                   call(e)   'TIS711C'
067100120611     C                   parm                    dscmz
067200120611     C                   parm      *blanks       esito
067300120611     C                   if        %error
067400120611     C                             or cmzerr = '1'
067500120611     C                             or esito  = '1'
067600120611     C                   eval      wrkesito = '3'
067700120611     C                   else
067800120611     C*
067900120611     C* 2° invio FIVAB
068000120611     C                   reset                   dscmz
068100120611     C                   move      vlrpoi        cmzdst
068200120611     C                   eval      cmzfld = vlrfou
068300120611     C                   eval      cmzmbd = vlrhdl
068400120611     C                   eval      %subst(cmzmbd:1:1) = 'M'
068500120611     C***                if        prmfir = *blanks
068600120611     C                   eval      cmzfla = 'FIVAB00F'
068700120611     C                   eval      cmzmba = 'FIVAB00F'
068800120611     C***                else
068900120611     C***                eval      cmzfla = prmfir
069000120611     C***                eval      cmzmba = prmfir
069100120611     C***                endif
069200120611     C                   eval      cmznrr = *zeros
069300120611     C                   move      §ctrokvb      cmznrr
069400120611     C                   eval      cmzlba = vlrfl1
069500120611     C                   call(e)   'TIS711C'
069600120611     C                   parm                    dscmz
069700120611     C                   parm      *blanks       esito
069800120611     C                   if        %error
069900120611     C                             or cmzerr = '1'
070000120611     C                             or esito  = '1'
070100120611     C                   eval      wrkesito = '3'
070200120611     C                   endif
070300120611     C                   endif
070400120611     C*
070500120611     C                   ENDSR
070600120611     C***
070700010601
070800010601
070900010601
071000010601      /TITLE Invio dei dati al punto operativo.
071100010601     C     opeini        BEGSR
071200010601     C*
071300010601     C* Inizializzo flag e contatori operativi
071400010601     C                   movel     '0'           flgGiro           1
071500010601     C                   movel     '0'           flgMulti          1
071600010601     C                   movel     '1'           flgStato          1
071700010615     C                   movel     '0'           flgOk             1
071800010601     C                   z-add     *zeros        cntNonEl         10 0
071900010601     C                   z-add     *zeros        depfil            3 0
072000010601     C                   z-add     *zeros        invfil            3 0
072100010601     C*
072200010601     C                   ENDSR
072300010601     C***
072400120611
072500120611
072600120611
072700120611     C     *pssr         BEGSR
072800120611     C*
072900120611     C                   if        %open(tivin00r)
073000120611     C                   close     tivin00r
073100120611     C                   endif
073200120611     C                   if        %open(fivabwwr)
073300120611     C                   close     fivabwwr
073400120611     C                   endif
073500120611     C                   if        %open(fivatwwr)
073600120611     C                   close     fivatwwr
073700120611     C                   endif
073800120611     C*
073900120611     C* Effettuo la chiamata al CLLE preposto
074000120611     C                   call(e)   'TITVVTC'
074100120611     C                   parm                    parccm
074200120611     C                   parm                    parmbr
074300120611     C                   parm      '2'           paropz
074400120611     C*
074500120611     C                   eval      wrkesito = '2'
074600120611     C*
074700120611     C                   seton                                        LR
074800120611     C*
074900120611     C                   ENDSR     '*CANCL'
075000120611     C***
075100010601
075200010330
075300010330
075400000613     C     *inzsr        BEGSR
075500990910     C*
075600990910     C     *entry        plist
075700990920     C                   parm                    tivlrds
075800990921     C                   parm      wrkesito      esito
075900000724     C                   parm                    prmlit
076000000710     C                   parm                    prmfir
076100010330     C*
076200010330     C* CALCOLA LA DATA CORRENTE
076300120611     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
076400120611     C                   eval      datcor = %dec(%date() : *ISO)
076500000613     C*
076600000613     C                   ENDSR
076700000613     C***
