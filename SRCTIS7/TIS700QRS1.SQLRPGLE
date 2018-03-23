000100990907      /TITLE Lettura log upload via Internet.
000200121217     H dftactgrp(*no) actgrp(*caller)
000300990907
000400000714     Ftivlr06l  uf   e           k disk    rename(tivlr000:tivlr600)
000500090415     Ftivlrlog  o  a e             disk    rename(tivlrlog:tivlrlog0)
000600990907
000700140801     D tivlrnrrp       s             10s 0
000800000714
000900000714     D* Informazioni sul job
001000000714     D psds           sds
001100000714     D  psjobname            244    253
001200000714     D  psuser               254    263
001300000714     D  psjobnbr             264    269s 0
001400010524
001500010524     D* DS di ridefinizione flag operativi sul log UPLOAD (tivlr00f)
001600010524     D dsvlr3        e ds
001700140801
001800140801     D* DS di ridefinizione file fisico letture SQL
001900140801     D tivlrds       e ds                  extname(tivlr00f) inz
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
004900140801     C*
005000140801     C/EXEC SQL
005100140801     C+ declare M1 cursor for
005200140801     C+ select tivlr00f.*, rrn(tivlr00f) from tivlr00f
005300140801     C+ where vlrFLG = '0' and substr(vlrFL3, 4, 2) = :inPROC
005400140801     C+ order by vlrFLG, substr(vlrFL3, 4, 2), vlrPRG,
005500140801     C+ vlrKSC, vlrTIP, vlrFL2
005600140801     C+ for read only
005700140801     C/END-EXEC
005800140801     C
005900140801     C/EXEC SQL
006000140801     C+ open M1
006100140801     C/END-EXEC
006200140801     C*
006300140801     C                   dou       sqlcod > *zeros OR *inrt OR *inlr
006400991109     C*
006500140801     C/EXEC SQL
006600140801     C+ Fetch M1 into :tivlrds, :tivlrnrrp
006700140801     C/END-EXEC
006800090415     C*
006900090415     C* Inizializzo buffer TIVLRLOG
007000090415     C                   setoff                                       55
007100090415     C                   clear                   vlrDATIE
007200090415     C                   clear                   vlrDATFE
007300090415     C                   clear                   vlrDATIP
007400090415     C                   clear                   vlrDATFP
007500991105     C*
007600140801     C* Gestione ENDJOB.
007700140801     C                   shtdn                                        lr
007800140801     C   lr              return
007900140801     C*
008000140801     C                   if        sqlcod = *zeros AND not *inlr
008100140801     C*
008200030613     C* Salvo subito il contenuto attuale del campo messaggio (se pieno a questo punto vuol dire che
008300030613     C* è stato scritto dalla procedura FTP/e-mail e riporta in nome del file ricevuto dal cliente)
008400030613     C                   if        %len(%trim(vlrmsg)) > 0
008500030613     C                   eval      savMSG = %trim(vlrmsg)
008600030613     C                   endif
008700140801     C*
008800990914     C* Se leggo un record a stato '0'
008900000717     C* e con i campi fondamentali pieni
009000990914     C* lo elaboro.
009100990908     C                   if        vlrflg = '0'
009200990921     C                             and vlrksc <> *blanks
009300990914     C                             and vlrhdl <> *blanks
009400070920     C*
009500070920     C* Salvo il numero sequenza del record attuale
009600070920     C                   eval      savSQZ = vlrSQZ
009700070920     C*
009800070920     C* Se è un record di una normale operazione "mono-file" oppure un rilancio da Gestione del Log
009900010524     C                   movel(p)  vlrfl3        dsvlr3
010000010524     C                   if        vlrfl2 = '01' or §vlr31 = '1'
010100000714     C                   exsr      exeproc
010200000714     C                   else
010300000714     C                   exsr      exemulti
010400000714     C                   endif
010500000725     C                   exsr      exepost
010600090415     C*
010700090415     C* Scrittura record TIVLRLOG
010800090415     C                   if        *in55 = *on and vlrpoi <> *zeros
010900140801     C                   eval      vlrRRN  = tivlrnrrp
011000090415     C                   eval      vlrPROC = %subst(vlrFL3:4:2)
011100090415     C                   eval      vlrNOMF = savMSG
011200090415     C                   eval      vlrPGI  = prmpgi
011300090415     C                   eval      *in54 = *off                                 *switch on/off write
011400090415     C   54              write     tivlrlog0
011500090415     C                   endif
011600991105     C*
011700991105     C                   endif
011800140801     C                   else
011900140801     C*
012000140804     C* Se fine file => chiusura
012100140804     C                   seton                                        lr
012200000713     C                   endif
012300991109     C*
012400991109     C                   enddo
012500140801     C*
012600140801     C/EXEC SQL
012700140801     C+ close M1
012800140801     C/END-EXEC
012900140801     C*
013000000714
013100000714
013200000714
013300000714      /TITLE Routine di controllo operazioni "multi-file"
013400000714     C*
013500000714     C     exemulti      begsr
013600000714     C*
013700000714     C                   z-add     *zeros        conta
013800000714     C                   movel     *zeros        contaa
013900000714     C*
014000000714     C/EXEC SQL
014100000830     C+ declare C1 cursor for select count(*)
014200000714     C+ from tivlr00f where
014300000714     C+ vlrprg = :vlrprg and vlrksc = :vlrksc
014400090504     C+ and vlrsqz = :vlrsqz and vlrflg <> 'M'
014500000714     C+ for fetch only
014600000714     C/END-EXEC
014700000714     C
014800000714     C/EXEC SQL
014900000714     C+ open C1
015000000714     C/END-EXEC
015100000714     C
015200000714     C/EXEC SQL
015300000714     C+ Fetch C1 into :conta
015400000714     C/END-EXEC
015500000714     C*
015600000714     C                   move      conta         contaa
015700000714     C*
015800000714     C                   if        sqlcod = *zeros and
015900000714     C                             contaa = vlrfl2
016000081010     C                   exsr      exeproc
016100081010     C                   endif
016200000724     C
016300000724     C/EXEC SQL
016400000724     C+ close C1
016500000724     C/END-EXEC
016600000724     C
016700000714     C*
016800000714     C                   endsr
016900000714
017000000714
017100000714
017200000714      /TITLE Routine di controllo operazioni "post-traduzione"
017300000714     C*
017400000714     C     exepost       begsr
017500000714     C*
017600000714     C                   z-add     *zeros        contaOK
017700000714     C                   movel     *zeros        contaaOK
017800000714     C                   z-add     *zeros        contaKO
017900000714     C*
018000000714     C/EXEC SQL
018100000830     C+ declare C2 cursor for select count(*)
018200000714     C+ from tivlr00f where
018300000714     C+ vlrprg = :vlrprg and vlrksc = :vlrksc
018400000725     C+ and vlrflg = '1'
018500000714     C+ for fetch only
018600000714     C/END-EXEC
018700000714     C
018800000714     C/EXEC SQL
018900000714     C+ open C2
019000000714     C/END-EXEC
019100000714     C
019200000714     C/EXEC SQL
019300000714     C+ Fetch C2 into :contaOK
019400000714     C/END-EXEC
019500000714     C*
019600000714     C*
019700000714     C/EXEC SQL
019800000830     C+ declare C3 cursor for select count(*)
019900000714     C+ from tivlr00f where
020000000714     C+ vlrprg = :vlrprg and vlrksc = :vlrksc
020100000725     C+ and vlrflg = '2'
020200000714     C+ for fetch only
020300000714     C/END-EXEC
020400000714     C
020500000714     C/EXEC SQL
020600000714     C+ open C3
020700000714     C/END-EXEC
020800000714     C
020900000714     C/EXEC SQL
021000000714     C+ Fetch C3 into :contaKO
021100000714     C/END-EXEC
021200000714     C*
021300000714     C                   move      contaOK       contaaOK
021400000714     C*
021500000714     C                   if        sqlcod = *zeros and
021600000714     C                             contaaOK = vlrfl2 and
021700000714     C                             contaKO = *zeros
021800000714     C                   exsr      exepgm
021900000714     C                   endif
022000000724     C
022100000724     C/EXEC SQL
022200000724     C+ close C2
022300000724     C/END-EXEC
022400000724     C
022500000724     C
022600000724     C/EXEC SQL
022700000724     C+ close C3
022800000724     C/END-EXEC
022900000724     C
023000000714     C*
023100000714     C                   endsr
023200000714
023300000714
023400000714      /TITLE Lancio l'eventuale PGM "post-traduzione"
023500000714     C*
023600000714     C     exepgm        begsr
023700000714     C* Lancio il pgm in variabile con relativi parametri
023800000714     C* (tutto ciò solo se chiaramente c'è qualcosa da lanciare)
023900000714    C                   if        prmpgi <> *blanks
024000090415     C*
024100090415     C* Valorizzazione file TIVLRLOG
024200090415     C*
024300090415     C/EXEC SQL
024400090415     C+ SET :vlrDATIP = NOW()
024500090415     C/END-EXEC
024600090415     C*
024700090415     C
024800000714     C* Chiamo il pgm post traduzione.
024900000714     C                   call(e)   'TIS700C1'
025000000714     C                   parm                    vlrprg
025100000714     C                   parm                    vlrksc
025200000714     C                   parm                    datime
025300000714     C                   parm                    prmpgi
025400000714     C                   parm                    prmppi
025500000717     C                   parm                    prmfit
025600000717     C                   parm                    prmfi3
025700000717     C                   parm                    prmfi4
025800000714     C                   parm      *blanks       prmesito
025900090415     C*
026000090415     C* Valorizzazione file TIVLRLOG
026100090415     C*
026200090415     C/EXEC SQL
026300090415     C+ SET :vlrDATFP = NOW()
026400090415     C/END-EXEC
026500090415     C*
026600090415     C
026700010523     C*
026800000714     C* Se la call è finita in errore
026900010523     C* o il lancio del PGM ha avuto degli errori (e comunque ha cambiato esito).
027000000714     C                   if        %error
027100010523     C                             or (prmesito <> '0' and prmesito <> *blanks)
027200000714     C     k01vlr        setll     tivlr06l
027300000714     C                   if        %found(tivlr06l)
027400000714     C     k01vlr        reade     tivlr06l
027500000714     C                   dow       not %eof
027600070920     C                   if        vlrsqz = savSQZ
027700000714     C                   if        vlrflg <> '2' or vlrflg <> '4'
027800000714     C                   eval      wrkmsg = 'Errori solo in post traduzione.'
027900000714     C                   else
028000000714     C                   eval      wrkmsg = 'Errori anche in post traduzione.'
028100000714     C                   endif
028200000714     C                   eval      vlrflg = '2'
028300000714     C                   eval      vlrmsg =
028400020909     C                               '(SEDE)'
028500000714     C                             + '/'
028600000714     C                             + %trim(psjobname)
028700000714     C                             + '/'
028800000714     C                             + %trim(psuser)
028900000714     C                             + '/'
029000000714     C                             + %trim(%editc(psjobnbr:'Z'))
029100000714     C                             + %trim(wrkmsg)
029200030613     C                   move      savMSG        vlrmsg
029300000714     C                   update    tivlr600
029400000714     C                   endif
029500000714     C     k01vlr        reade     tivlr06l
029600000714     C                   enddo
029700000714     C                   endif
029800000714     C                   endif
029900000714     C*
030000000714     C                   endif
030100000714     C*
030200000714     C                   endsr
030300000714
030400000714
030500000714
030600000714
030700000717      /TITLE Routine di avio processamento record di log
030800000714     C*
030900000714     C     exeproc       begsr
031000090415     C*
031100090415     C                   seton                                        55
031200090415     C*
031300090415     C* Valorizzazione file TIVLRLOG
031400090415     C*
031500090415     C/EXEC SQL
031600090415     C+ SET :vlrDATIE = NOW()
031700090415     C/END-EXEC
031800090415     C*
031900090415     C
032000000714     C*
032100000714     C                   eval      prmpgi = *blanks
032200000714     C                   eval      prmppi = *blanks
032300000714     C                   call(e)   'TIS701R'
032400140801     C                   parm                    tivlrnrrp
032500010820     C                   parm      *blanks       prmpgi
032600010820     C                   parm      *blanks       prmppi
032700010820     C                   parm      *blanks       prmfit
032800010820     C                   parm      *blanks       prmfi3
032900010820     C                   parm      *blanks       prmfi4
033000090415     C*
033100090415     C* Valorizzazione file TIVLRLOG
033200090415     C*
033300090415     C/EXEC SQL
033400090415     C+ SET :vlrDATFE = NOW()
033500090415     C/END-EXEC
033600090415     C*
033700090415     C
033800000714     C*
033900000714     C                   endsr
034000000714
034100000714
034200000714      /TITLE Routine di *init PGM
034300000714     C*
034400000714     C     *inzsr        begsr
034500140801     C*
034600140801     C     *entry        plist
034700140801     C                   parm                    inPROC            2
034800000714     C*
034900000714     C     K01vlr        klist
035000000714     C                   kfld                    vlrprg
035100000714     C                   kfld                    vlrksc
035200000714     C*
035300000714     C                   time                    wrkoggiiso
035400000714     C                   time                    wrkora
035500000714     C     *iso          move      wrkoggiiso    wrkoggi
035600000714     C                   move(p)   wrkora        datime
035700000714     C                   movel     wrkoggi       datime
035800000714     C*
035900000714     C                   endsr
036000000714
