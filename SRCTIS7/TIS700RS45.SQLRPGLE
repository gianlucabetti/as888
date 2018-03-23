000100990907      /TITLE Lettura log upload via Internet.
000200121217     H dftactgrp(*no) actgrp(*caller)
000300990907
000400130725     Ftivlrp45l if   e           k disk    infds(tivlrinf)
000500000714     Ftivlr06l  uf   e           k disk    rename(tivlr000:tivlr600)
000600090415     Ftivlrlog  o  a e             disk    rename(tivlrlog:tivlrlog0)
000700990907
000800990908     D tivlrinf        ds
000900990908     D  tivlrnrri            397    400i 0
001000990910     D tivlrnrrp       s             10s 0
001100000714
001200000714     D* Informazioni sul job
001300000714     D psds           sds
001400000714     D  psjobname            244    253
001500000714     D  psuser               254    263
001600000714     D  psjobnbr             264    269s 0
001700010524
001800010524     D* DS di ridefinizione flag operativi sul log UPLOAD (tivlr00f)
001900010524     D dsvlr3        e ds
002000990907
002100000710     D* variabili di work
002200000710     D  prmpgi         s             10
002300000710     D  prmppi         s             50
002400000717     D  prmfit         s             10
002500000717     D  prmfi3         s             10
002600000717     D  prmfi4         s             10
002700000714     D  prmesito       s              1    inz(*blanks)
002800000714     D  wrkmsg         s             80    inz(*blanks)
002900000713     D  conta          s              2  0
003000000713     D  contaa         s              2
003100000714     D  contaOK        s              2  0
003200000714     D  contaaOK       s              2
003300000714     D  contaKO        s              2  0
003400000714     D  wrkoggiiso     s               d
003500000714     D  wrkoggi        s              8  0
003600000714     D  wrkora         s              6  0
003700000714     D  datime         s             14
003800030616     D  savMSG         s             30
003900070920     D  savSQZ         s                   like(vlrsqz) inz(*loval)
004000000710
004100081010     C*
004200081010     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
004300081010     C
004400081010     C/EXEC SQL
004500081010     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
004600081010     C/END-EXEC
004700081010     C*
004800130617     C                   setoff                                       lrrt
004900130617     C     *start        setll     tivlrp45l
005000130614     C                   dou       *inlr or *inrt
005100991109     C*
005200130614     C                   read      tivlrp45l                              rt
005300090415     C*
005400090415     C* Inizializzo buffer TIVLRLOG
005500090415     C                   setoff                                       55
005600090415     C                   clear                   vlrDATIE
005700090415     C                   clear                   vlrDATFE
005800090415     C                   clear                   vlrDATIP
005900090415     C                   clear                   vlrDATFP
006000991105     C*
006100130614     C                   if        not *inlr and not *inrt
006200991111     C* Gestione ENDJOB.
006300991111     C                   shtdn                                        lr
006400991111     C   lr              return
006500030613     C* Salvo subito il contenuto attuale del campo messaggio (se pieno a questo punto vuol dire che
006600030613     C* è stato scritto dalla procedura FTP/e-mail e riporta in nome del file ricevuto dal cliente)
006700030613     C                   if        %len(%trim(vlrmsg)) > 0
006800030613     C                   eval      savMSG = %trim(vlrmsg)
006900030613     C                   endif
007000990914     C* Se leggo un record a stato '0'
007100000717     C* e con i campi fondamentali pieni
007200990914     C* lo elaboro.
007300990908     C                   if        vlrflg = '0'
007400990921     C                             and vlrksc <> *blanks
007500990914     C                             and vlrhdl <> *blanks
007600070920     C*
007700070920     C* Salvo il numero sequenza del record attuale
007800070920     C                   eval      savSQZ = vlrSQZ
007900070920     C*
008000070920     C* Se è un record di una normale operazione "mono-file" oppure un rilancio da Gestione del Log
008100010524     C                   movel(p)  vlrfl3        dsvlr3
008200010524     C                   if        vlrfl2 = '01' or §vlr31 = '1'
008300000714     C                   exsr      exeproc
008400000714     C                   else
008500000714     C                   exsr      exemulti
008600000714     C                   endif
008700000725     C                   exsr      exepost
008800090415     C*
008900090415     C* Scrittura record TIVLRLOG
009000090415     C                   if        *in55 = *on and vlrpoi <> *zeros
009100090415     C                   eval      vlrRRN  = tivlrnrri
009200090415     C                   eval      vlrPROC = %subst(vlrFL3:4:2)
009300090415     C                   eval      vlrNOMF = savMSG
009400090415     C                   eval      vlrPGI  = prmpgi
009500090415     C                   eval      *in54 = *off                                 *switch on/off write
009600090415     C   54              write     tivlrlog0
009700090415     C                   endif
009800991105     C*
009900991105     C                   endif
010000000713     C                   endif
010100991109     C*
010200991109     C                   enddo
010300000714
010400000714
010500000714
010600000714      /TITLE Routine di controllo operazioni "multi-file"
010700000714     C*
010800000714     C     exemulti      begsr
010900000714     C*
011000000714     C                   z-add     *zeros        conta
011100000714     C                   movel     *zeros        contaa
011200000714     C*
011300000714     C/EXEC SQL
011400000830     C+ declare C1 cursor for select count(*)
011500000714     C+ from tivlr00f where
011600000714     C+ vlrprg = :vlrprg and vlrksc = :vlrksc
011700090504     C+ and vlrsqz = :vlrsqz and vlrflg <> 'M'
011800000714     C+ for fetch only
011900000714     C/END-EXEC
012000000714     C
012100000714     C/EXEC SQL
012200000714     C+ open C1
012300000714     C/END-EXEC
012400000714     C
012500000714     C/EXEC SQL
012600000714     C+ Fetch C1 into :conta
012700000714     C/END-EXEC
012800000714     C*
012900000714     C                   move      conta         contaa
013000000714     C*
013100000714     C                   if        sqlcod = *zeros and
013200000714     C                             contaa = vlrfl2
013300081010     C*
013400090415     C*                  z-add     *zeros        minRrnPrg        15 0
013500081010     C*
013600090415     C*/EXEC SQL
013700090415     C*+ declare C4 cursor for select min(rrn(tivlr00f))
013800090415     C*+ from tivlr00f where
013900090415     C*+ vlrprg = :vlrprg and vlrksc = :vlrksc and
014000090415     C*+ vlrflg = '0'
014100090415     C*+ for fetch only
014200090415     C*/END-EXEC
014300081010     C
014400090415     C*/EXEC SQL
014500090415     C*+ open C4
014600090415     C*/END-EXEC
014700081010     C
014800090415     C*/EXEC SQL
014900090415     C*+ Fetch C4 into :minRrnPrg
015000090415     C*/END-EXEC
015100081010     C
015200090415     C*/EXEC SQL
015300090415     C*+ close C4
015400090415     C*/END-EXEC
015500081010     C
015600081010     C
015700090415     C***                if        minRrnPrg = tivlrnrri
015800081010     C                   exsr      exeproc
015900090415     C***                endif
016000081010     C                   endif
016100000724     C
016200000724     C/EXEC SQL
016300000724     C+ close C1
016400000724     C/END-EXEC
016500000724     C
016600000714     C*
016700000714     C                   endsr
016800000714
016900000714
017000000714
017100000714      /TITLE Routine di controllo operazioni "post-traduzione"
017200000714     C*
017300000714     C     exepost       begsr
017400000714     C*
017500000714     C                   z-add     *zeros        contaOK
017600000714     C                   movel     *zeros        contaaOK
017700000714     C                   z-add     *zeros        contaKO
017800000714     C*
017900000714     C/EXEC SQL
018000000830     C+ declare C2 cursor for select count(*)
018100000714     C+ from tivlr00f where
018200000714     C+ vlrprg = :vlrprg and vlrksc = :vlrksc
018300000725     C+ and vlrflg = '1'
018400000714     C+ for fetch only
018500000714     C/END-EXEC
018600000714     C
018700000714     C/EXEC SQL
018800000714     C+ open C2
018900000714     C/END-EXEC
019000000714     C
019100000714     C/EXEC SQL
019200000714     C+ Fetch C2 into :contaOK
019300000714     C/END-EXEC
019400000714     C*
019500000714     C*
019600000714     C/EXEC SQL
019700000830     C+ declare C3 cursor for select count(*)
019800000714     C+ from tivlr00f where
019900000714     C+ vlrprg = :vlrprg and vlrksc = :vlrksc
020000000725     C+ and vlrflg = '2'
020100000714     C+ for fetch only
020200000714     C/END-EXEC
020300000714     C
020400000714     C/EXEC SQL
020500000714     C+ open C3
020600000714     C/END-EXEC
020700000714     C
020800000714     C/EXEC SQL
020900000714     C+ Fetch C3 into :contaKO
021000000714     C/END-EXEC
021100000714     C*
021200000714     C                   move      contaOK       contaaOK
021300000714     C*
021400000714     C                   if        sqlcod = *zeros and
021500000714     C                             contaaOK = vlrfl2 and
021600000714     C                             contaKO = *zeros
021700000714     C                   exsr      exepgm
021800000714     C                   endif
021900000724     C
022000000724     C/EXEC SQL
022100000724     C+ close C2
022200000724     C/END-EXEC
022300000724     C
022400000724     C
022500000724     C/EXEC SQL
022600000724     C+ close C3
022700000724     C/END-EXEC
022800000724     C
022900000714     C*
023000000714     C                   endsr
023100000714
023200000714
023300000714      /TITLE Lancio l'eventuale PGM "post-traduzione"
023400000714     C*
023500000714     C     exepgm        begsr
023600000714     C* Lancio il pgm in variabile con relativi parametri
023700000714     C* (tutto ciò solo se chiaramente c'è qualcosa da lanciare)
023800000714    C                   if        prmpgi <> *blanks
023900090415     C*
024000090415     C* Valorizzazione file TIVLRLOG
024100090415     C*
024200090415     C/EXEC SQL
024300090415     C+ SET :vlrDATIP = NOW()
024400090415     C/END-EXEC
024500090415     C*
024600090415     C
024700000714     C* Chiamo il pgm post traduzione.
024800000714     C                   call(e)   'TIS700C1'
024900000714     C                   parm                    vlrprg
025000000714     C                   parm                    vlrksc
025100000714     C                   parm                    datime
025200000714     C                   parm                    prmpgi
025300000714     C                   parm                    prmppi
025400000717     C                   parm                    prmfit
025500000717     C                   parm                    prmfi3
025600000717     C                   parm                    prmfi4
025700000714     C                   parm      *blanks       prmesito
025800090415     C*
025900090415     C* Valorizzazione file TIVLRLOG
026000090415     C*
026100090415     C/EXEC SQL
026200090415     C+ SET :vlrDATFP = NOW()
026300090415     C/END-EXEC
026400090415     C*
026500090415     C
026600010523     C*
026700000714     C* Se la call è finita in errore
026800010523     C* o il lancio del PGM ha avuto degli errori (e comunque ha cambiato esito).
026900000714     C                   if        %error
027000010523     C                             or (prmesito <> '0' and prmesito <> *blanks)
027100000714     C     k01vlr        setll     tivlr06l
027200000714     C                   if        %found(tivlr06l)
027300000714     C     k01vlr        reade     tivlr06l
027400000714     C                   dow       not %eof
027500070920     C                   if        vlrsqz = savSQZ
027600000714     C                   if        vlrflg <> '2' or vlrflg <> '4'
027700000714     C                   eval      wrkmsg = 'Errori solo in post traduzione.'
027800000714     C                   else
027900000714     C                   eval      wrkmsg = 'Errori anche in post traduzione.'
028000000714     C                   endif
028100000714     C                   eval      vlrflg = '2'
028200000714     C                   eval      vlrmsg =
028300020909     C                               '(SEDE)'
028400000714     C                             + '/'
028500000714     C                             + %trim(psjobname)
028600000714     C                             + '/'
028700000714     C                             + %trim(psuser)
028800000714     C                             + '/'
028900000714     C                             + %trim(%editc(psjobnbr:'Z'))
029000000714     C                             + %trim(wrkmsg)
029100030613     C                   move      savMSG        vlrmsg
029200000714     C                   update    tivlr600
029300000714     C                   endif
029400000714     C     k01vlr        reade     tivlr06l
029500000714     C                   enddo
029600000714     C                   endif
029700000714     C                   endif
029800000714     C*
029900000714     C                   endif
030000000714     C*
030100000714     C                   endsr
030200000714
030300000714
030400000714
030500000714
030600000717      /TITLE Routine di avio processamento record di log
030700000714     C*
030800000714     C     exeproc       begsr
030900090415     C*
031000090415     C                   seton                                        55
031100090415     C*
031200090415     C* Valorizzazione file TIVLRLOG
031300090415     C*
031400090415     C/EXEC SQL
031500090415     C+ SET :vlrDATIE = NOW()
031600090415     C/END-EXEC
031700090415     C*
031800090415     C
031900000714     C*
032000000714     C                   eval      prmpgi = *blanks
032100000714     C                   eval      prmppi = *blanks
032200000714     C                   call(e)   'TIS701R'
032300000714     C                   parm      tivlrnrri     tivlrnrrp
032400010820     C                   parm      *blanks       prmpgi
032500010820     C                   parm      *blanks       prmppi
032600010820     C                   parm      *blanks       prmfit
032700010820     C                   parm      *blanks       prmfi3
032800010820     C                   parm      *blanks       prmfi4
032900090415     C*
033000090415     C* Valorizzazione file TIVLRLOG
033100090415     C*
033200090415     C/EXEC SQL
033300090415     C+ SET :vlrDATFE = NOW()
033400090415     C/END-EXEC
033500090415     C*
033600090415     C
033700000714     C*
033800000714     C                   endsr
033900000714
034000000714
034100000714      /TITLE Routine di *init PGM
034200000714     C*
034300000714     C     *inzsr        begsr
034400000714     C*
034500000714     C     K01vlr        klist
034600000714     C                   kfld                    vlrprg
034700000714     C                   kfld                    vlrksc
034800000714     C*
034900000714     C                   time                    wrkoggiiso
035000000714     C                   time                    wrkora
035100000714     C     *iso          move      wrkoggiiso    wrkoggi
035200000714     C                   move(p)   wrkora        datime
035300000714     C                   movel     wrkoggi       datime
035400000714     C*
035500000714     C                   endsr
035600000714
