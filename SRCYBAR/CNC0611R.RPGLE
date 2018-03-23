000100991216      /TITLE Gestione automatica CNNUM00F.
000200991215
000300991215      **********************************************************************
000400991215      * ESI0611
000500991215      * '0' = Nessun errore.
000600991215      * 'A' = Il valore di OPZ0611 non è valido.
000700991215      * 'B' = Errori formali nei parametri ricevuti.
000800991215      * 'C' = Operazione non riuscita.
000900991217      * 'D' = Esistono già dei records per il nuovo anno.
001000991215      **********************************************************************
001100991215
001200991215     H dftactgrp(*no) actgrp(*caller)
001300991215
001400991215     Fcnnum00f  uf a e           k disk    usropn
001500991215
001600991216     D cnc0611ds     e ds
001700991215     D keyaac          s                   like(numaac)
001800991215     D keyrgs          s                   like(numrgs)
001900991215     D keylib          s                   like(numlib)
002000991215     D wrkkeynbr       s              1  0
002100991215     D wrknumaac       s              4  0
002200991215     D wrkaacliminf    s              4  0 inz(1940)
002300991215     D wrkaaclimsup    s              4  0
002400991215
002500991215     C                   exsr      rcvparms
002600991215     C*
002700991215     C                   if        not %open(cnnum00f)
002800991215     C                   open      cnnum00f
002900991215     C                   endif
003000991215     C*
003100991215     C                   select
003200991215     C* Immissione.
003300991215     C                   when      opz0611 = '1'
003400991215     C                   exsr      immcnnum
003500991215     C* Modifica.
003600991215     C                   when      opz0611 = '2'
003700991215     C                   exsr      modcnnum
003800991215     C* Copia.
003900991215     C                   when      opz0611 = '3'
004000991215     C                   exsr      cpycnnum
004100991215     C* Cancellazione.
004200991215     C                   when      opz0611 = '4'
004300991215     C                   exsr      cancnnum
004400991215     C* Riorganizzazione.
004500991215     C                   when      opz0611 = '5'
004600991215     C                   exsr      rgzcnnum
004700991215     C                   endsl
004800991215     C*
004900991215     C                   if        %open(cnnum00f)
005000991215     C                   close     cnnum00f
005100991215     C                   endif
005200991215     C* Arrivederci.
005300991215     C                   eval      esi0611 = '0'
005400991215     C                   exsr      uscita
005500991215
005600991215      /TITLE Immissione.
005700991215     C     immcnnum      begsr
005800991215     C*
005900991216     C                   move      aacn0611      keyaac
006000991216     C                   move      rgsn0611      keyrgs
006100991216     C                   move      libn0611      keylib
006200991216     C     k03num00      setll     cnnum00f
006300991216     C*
006400991216     C                   if        %equal(cnnum00f)
006500991216     C* Se la chiave esiste già, errore.
006600991216     C                   eval      esi0611 = 'C'
006700991216     C                   exsr      uscita
006800991216     C*
006900991216     C                   else
007000991216     C*
007100991215     C                   move      aacn0611      numaac
007200991215     C                   move      rgsn0611      numrgs
007300991215     C                   move      libn0611      numlib
007400991215     C*
007500991215     C                   exsr      wrtcnnum000
007600991216     C*
007700991216     C                   endif
007800991215     C*
007900991215     C                   endsr
008000991215
008100991215      /TITLE Modifica.
008200991215     C     modcnnum      begsr
008300991215     C*
008400991215     C                   exsr      setkeycnnum
008500991215     C                   exsr      setllcnnum
008600991215     C*
008700991215     C                   dou       %eof(cnnum00f)
008800991215     C*
008900991215     C                   exsr      readcnnum
009000991215     C*
009100991215     C                   if        not %eof(cnnum00f)
009200991215     C*
009300991215     C                   exsr      updcnnum000
009400991215     C*
009500991215     C                   endif
009600991215     C*
009700991215     C                   enddo
009800991215     C*
009900991215     C                   endsr
010000991215
010100991215      /TITLE Copia.
010200991215     C     cpycnnum      begsr
010300991227     C* Se è stata chiesta la copia dal 1999 al 2000
010400991227     C* cancello i vecchi record con anno 00
010500991227     C* che servivano per attivare la numerazione automatica.
010600991227     C                   if        aacv0611 = 1999
010700991227     C                             and aacn0611 = 2000
010800991227     C                   eval      keyaac = 0
010900991227     C                   dou       not %found(cnnum00f)
011000991227     C                   dou       not %error
011100991227     C     k01num00      delete(e) cnnum00f
011200991227     C                   enddo
011300991227     C                   enddo
011400991227     C                   endif
011500991216     C* Preparo la chiave per il controllo di non esistenza del nuovo anno.
011600991216     C                   move      aacn0611      keyaac
011700991216     C* Controllo che per il nuovo anno non esistano records.
011800991216     C     k01num00      setll     cnnum00f
011900991216     C*
012000991216     C                   if        %equal(cnnum00f)
012100991216     C* Se per il nuovo anno esistono già dei records, errore.
012200991217     C                   eval      esi0611 = 'D'
012300991216     C                   exsr      uscita
012400991216     C*
012500991216     C                   else
012600991215     C*
012700991215     C                   exsr      setkeycnnum
012800991215     C                   exsr      setllcnnum
012900991215     C*
013000991215     C                   dou       %eof(cnnum00f)
013100991215     C*
013200991215     C                   exsr      readcnnum
013300991215     C*
013400991215     C                   if        not %eof(cnnum00f)
013500991216     C*
013600991215     C                   move      aacn0611      numaac
013700991215     C*
013800991215     C                   exsr      wrtcnnum000
013900991215     C*
014000991215     C                   endif
014100991215     C*
014200991215     C                   enddo
014300991216     C*
014400991216     C                   endif
014500991215     C*
014600991215     C                   endsr
014700991215
014800991215      /TITLE Cancellazione.
014900991215     C     cancnnum      begsr
015000991215     C*
015100991215     C                   exsr      setkeycnnum
015200991215     C                   exsr      setllcnnum
015300991215     C*
015400991215     C                   dou       %eof(cnnum00f)
015500991215     C*
015600991215     C                   exsr      readcnnum
015700991215     C*
015800991215     C                   if        not %eof(cnnum00f)
015900991215     C*
016000991216     C                   exsr      dltcnnum000
016100991215     C*
016200991215     C                   endif
016300991215     C*
016400991215     C                   enddo
016500991215     C*
016600991215     C                   endsr
016700991215
016800991215      /TITLE Riorganizzazione.
016900991215     C     rgzcnnum      begsr
017000991215     C*
017100991215     C                   eval      wrkkeynbr = 0
017200991215     C                   eval      wrkaaclimsup = aacv0611
017300991215     C*
017400991215     C                   exsr      setllcnnum
017500991215     C*
017600991215     C                   dou       %eof(cnnum00f)
017700991215     C*
017800991215     C                   exsr      readcnnum
017900991215     C*
018000991215     C                   if        not %eof(cnnum00f)
018100991215     C                             and wrknumaac >= wrkaacliminf
018200991215     C                             and wrknumaac <= wrkaaclimsup
018300991215     C*
018400991216     C                   exsr      dltcnnum000
018500991215     C*
018600991215     C                   endif
018700991215     C*
018800991215     C                   enddo
018900991215     C*
019000991215     C                   endsr
019100991215
019200991215      /TITLE Scrittura CNNUM000.
019300991215     C     wrtcnnum000   begsr
019400991215     C*
019500991215     C                   eval      numnum = numn0611
019600991215     C                   eval      numdtp = dtpn0611
019700991215     C*
019800991215     C                   write(e)  cnnum000
019900991215     C*
020000991215     C                   if        %error
020100991215     C                   eval      esi0611 = 'C'
020200991215     C                   endif
020300991215     C*
020400991215     C                   endsr
020500991215
020600991215      /TITLE Aggiornamento CNNUM000.
020700991215     C     updcnnum000   begsr
020800991215     C*
020900991215     C                   eval      numnum = numn0611
021000991215     C                   eval      numdtp = dtpn0611
021100991215     C*
021200991215     C                   update(e) cnnum000
021300991215     C*
021400991215     C                   if        %error
021500991215     C                   eval      esi0611 = 'C'
021600991215     C                   endif
021700991215     C*
021800991215     C                   endsr
021900991216
022000991216      /TITLE Cancellazione CNNUM000.
022100991216     C     dltcnnum000   begsr
022200991216     C*
022300991216     C                   delete(e) cnnum000
022400991216     C*
022500991216     C                   if        %error
022600991216     C                   eval      esi0611 = 'C'
022700991216     C                   endif
022800991216     C*
022900991216     C                   endsr
023000991215
023100991215      /TITLE Lettura CNNUM00F.
023200991215     C     readcnnum     begsr
023300991215     C*
023400991215     C                   select
023500991215     C                   when      wrkkeynbr = 0
023600991215     C                   read      cnnum00f
023700991215     C                   when      wrkkeynbr = 1
023800991215     C     k01num00      reade     cnnum00f
023900991215     C                   when      wrkkeynbr = 2
024000991215     C     k02num00      reade     cnnum00f
024100991215     C                   when      wrkkeynbr = 3
024200991215     C     k03num00      reade     cnnum00f
024300991215     C                   endsl
024400991215     C*
024500991215     C                   if        not %eof
024600991215     C                   if        numaac < 40
024700991215     C                   eval      wrknumaac = numaac + 2000
024800991215     C                   else
024900991215     C                   eval      wrknumaac = numaac + 1900
025000991215     C                   endif
025100991215     C                   endif
025200991215     C*
025300991215     C                   endsr
025400991215
025500991215      /TITLE Posizionamento CNNUM00F.
025600991215     C     setllcnnum    begsr
025700991215     C*
025800991215     C                   select
025900991215     C                   when      wrkkeynbr = 0
026000991215     C     *loval        setll     cnnum00f
026100991215     C                   when      wrkkeynbr = 1
026200991215     C     k01num00      setll     cnnum00f
026300991215     C                   when      wrkkeynbr = 2
026400991215     C     k02num00      setll     cnnum00f
026500991215     C                   when      wrkkeynbr = 3
026600991215     C     k03num00      setll     cnnum00f
026700991215     C                   endsl
026800991215     C*
026900991215     C                   endsr
027000991215
027100991215      /TITLE Imposto chiave CNNUM00F.
027200991215     C     setkeycnnum   begsr
027300991215     C*
027400991215     C                   reset                   wrkkeynbr
027500991215     C                   reset                   keyaac
027600991215     C                   reset                   keyrgs
027700991215     C                   reset                   keylib
027800991215     C*
027900991215     C                   select
028000991215     C*
028100991215     C                   when      aacv0611 <> 0
028200991215     C                             and rgsv0611 <> *blanks
028300991215     C                             and libv0611 <> *blanks
028400991215     C                   eval      wrkkeynbr = 3
028500991215     C                   move      aacv0611      keyaac
028600991215     C                   move      rgsv0611      keyrgs
028700991215     C                   move      libv0611      keylib
028800991215     C*
028900991215     C                   when      aacv0611 <> 0
029000991215     C                             and rgsv0611 <> *blanks
029100991215     C                             and libv0611 =  *blanks
029200991215     C                   eval      wrkkeynbr = 2
029300991215     C                   move      aacv0611      keyaac
029400991215     C                   move      rgsv0611      keyrgs
029500991215     C*
029600991215     C                   when      aacv0611 <> 0
029700991215     C                             and rgsv0611 =  *blanks
029800991215     C                             and libv0611 =  *blanks
029900991215     C                   eval      wrkkeynbr = 1
030000991215     C                   move      aacv0611      keyaac
030100991215     C*
030200991215     C                   endsl
030300991215     C*
030400991215     C                   endsr
030500991215
030600991215      /TITLE Ricevo e controllo i parametri.
030700991215     C     rcvparms      begsr
030800991215     C*
030900991215     C                   clear                   esi0611
031000991215     C* Controllo formale dei parametri ricevuti.
031100991215     C                   testn                   rgsv0611             01  01
031200991215     C                   if        not *in01
031300991215     C                   eval      esi0611 = 'B'
031400991215     C                   exsr      uscita
031500991215     C                   endif
031600991215     C                   testn                   libv0611             01  01
031700991215     C                   if        not *in01
031800991215     C                   eval      esi0611 = 'B'
031900991215     C                   exsr      uscita
032000991215     C                   endif
032100991215     C                   testn                   rgsn0611             01  01
032200991215     C                   if        not *in01
032300991215     C                   eval      esi0611 = 'B'
032400991215     C                   exsr      uscita
032500991215     C                   endif
032600991215     C                   testn                   libn0611             01  01
032700991215     C                   if        not *in01
032800991215     C                   eval      esi0611 = 'B'
032900991215     C                   exsr      uscita
033000991215     C                   endif
033100991217     C*
033200991217     C                   if        dtpn0611 = 0
033300991217     C                   eval      dtpn0611 = 000101
033400991217     C                   endif
033500991215     C* Controllo di congruenza dei parametri ricevuti.
033600991215     C                   select
033700991215     C* Immissione.
033800991215     C                   when      opz0611 = '1'
033900991215     C                   if        aacn0611 = 0
034000991215     C                             or rgsn0611 = *blanks
034100991215     C                             or libn0611 = *blanks
034200991215     C                   eval      esi0611 = 'B'
034300991215     C                   exsr      uscita
034400991215     C                   endif
034500991215     C* Modifica.
034600991215     C                   when      opz0611 = '2'
034700991215     C* Copia.
034800991215     C                   when      opz0611 = '3'
034900991216     C                   if        aacv0611 = 0
035000991216     C                   select
035100991216     C                   when      *month = 1
035200991216     C                   eval      aacv0611 = *year - 1
035300991216     C                   when      *month = 12
035400991216     C                   eval      aacv0611 = *year
035500991216     C                   other
035600991216     C                   eval      esi0611 = 'B'
035700991216     C                   exsr      uscita
035800991216     C                   endsl
035900991216     C                   endif
036000991216     C                   if        aacn0611 = 0
036100991216     C                   eval      aacn0611 = aacv0611 + 1
036200991216     C                   endif
036300991215     C* Cancellazione.
036400991215     C                   when      opz0611 = '4'
036500991215     C* Riorganizzazione.
036600991215     C                   when      opz0611 = '5'
036700991215     C                   if        aacv0611 = 0
036800991215     C                   select
036900991215     C                   when      *month = 1
037000991215     C                   eval      aacv0611 = *year - 2
037100991215     C                   when      *month = 12
037200991215     C                   eval      aacv0611 = *year - 1
037300991215     C                   other
037400991215     C                   eval      esi0611 = 'B'
037500991215     C                   exsr      uscita
037600991215     C                   endsl
037700991215     C                   endif
037800991215     C* Non previsto.
037900991215     C                   other
038000991215     C                   eval      esi0611 = 'A'
038100991215     C                   exsr      uscita
038200991215     C                   endsl
038300991215     C*
038400991215     C                   endsr
038500991215
038600991215      /TITLE Operazioni finali.
038700991215     C     uscita        begsr
038800991215     C*
038900991216     C                   if        rtlr0611 = 'L'
039000991216     C                   seton                                        lr
039100991216     C                   endif
039200991216     C                   return
039300991215     C*
039400991215     C                   endsr
039500991215
039600991215      /TITLE Operazioni iniziali.
039700991215     C     *inzsr        begsr
039800991215     C*
039900991215     C     *entry        plist
040000991216     C                   parm                    cnc0611ds
040100991215     C*
040200991215     C     k01num00      klist
040300991215     C                   kfld                    keyaac
040400991215     C     k02num00      klist
040500991215     C                   kfld                    keyaac
040600991215     C                   kfld                    keyrgs
040700991215     C     k03num00      klist
040800991215     C                   kfld                    keyaac
040900991215     C                   kfld                    keyrgs
041000991215     C                   kfld                    keylib
041100991215     C*
041200991215     C                   endsr
