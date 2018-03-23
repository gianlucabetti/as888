000100011029     H DECEDIT('0,') DATEDIT(*DMY.)
000200100514     H* TRULNRSR *---------------------------------------------------*
000300100514     H*   - CONTROLLO numero serie e segnacolli dal / al cliente
000400060704     H*--------------------------------------------------------------*
000500060704     ftabel00f  if   e           k disk
000600100514     ftntbe01l  if   e           k disk
000700060704     fazorg01l  if   e           k disk
000800100514     D MSG             S             78    DIM(10) CTDATA PERRCD(1)
000900011019
001000100514     d ds3C          e ds
001100100514     d d3CP          e ds
001200100514     d d3EW          e ds
001300100514     d trulNRSDS     e ds
001400100514
001500011029     d tibs02DS      e ds
001600011030
001700100514     d §kut            s                   like(tblkut) inz(1)
001800100514     d §cod            s                   like(tblcod)
001900100514     d COD             s                   like(tblcod)
002000100514     d §key            s                   like(tblkey)
002100100514     d KEY             s                   like(tblkey)
002200100514     d KE1             s                   like(tbeke1)
002300100514     d W007a           s              7
002400100514     d SerieOK         s              1
002500100514     d FlsOK           s              1
002600100514     d NO_range        s              1
002700100514     d w3CPnrs         s              2  0
002800100514     d wparz_d         s                   like(inrsncd)
002900100514     d wparz_a         s                   like(inrsnca)
003000011019
003100011019      *----------------------------------------------------------------
003200060704     C     KTAB          KLIST
003300060704     C                   KFLD                    §kut
003400060704     C                   KFLD                    COD
003500060704     C                   KFLD                    KEY
003600060704      *----------------------------------------------------------------
003700011029     c     *entry        plist
003800100514     c                   parm                    trulNRSDS
003900011106
004000100514      /free
004100011019
004200100514       clear      oNRSERR    ;
004300100514       clear      oNRSMSG    ;
004400111216       clear      oNRSPRZ    ;
004500100514       if    inrsnca=0   ;
004600100514        inrsnca=inrsncd  ;
004700100514       endif   ;
004800011019
004900100514 1     if iNRSTLa<>'C'    ;
005000100514
005100100514 2     select   ;
005200100514       when  iNRSccm=0   ;
005300100514
005400100514       onrsMSG= msg(1) ;
005500100514       onrsERR= 'E'   ;
005600100514
005700100514       when  iNRSnrs=0   ;
005800100514
005900100514       onrsMSG= msg(2) ;
006000100514       onrsERR= 'S'   ;
006100100514 x2    other          ;
006200100514
006300100514       exsr Controlla ;
006400100514 2     endsl          ;
006500100514 1     endif ;
006600100514
006700100514 1     if        inrstla <> *blanks     ;
006800100514        *inlr = *on           ;
006900100514       else  ;
007000100514        *inrt = *on           ;
007100100514 1     endif ;
007200100514
007300100514       //-------------------------------------------------------
007400100514       //   controlla serie
007500100514       //-------------------------------------------------------
007600100514       BEGSR  Controlla     ;
007700100514
007800100514       clear  key ;
007900100514       key =%editc(inrsccm:'X')  ;
008000100514
008100100514       chain  (1:'3C':key) tabel00f  ;
008200100514
008300100514       if   not %found(tabel00f) or tblflg<>' '  ;
008400100514       onrsMSG= msg(3) ;
008500100514       onrsERR= 'E'   ;
008600100514       leavesr  ;
008700100514       endif   ;
008800100514
008900100514       ds3c=tbluni    ;
009000100514
009100100517       // Se tipo scarico EASY WEB vado subito sulla tabella 3EW
009200100514
009300100514       if §3ccba='ESWEB'   ;
009400100514        exsr Tabella3EW   ;
009500100514        else  ;
009600100514        exsr Tabella3CP   ;
009700100514        endif   ;
009800100514
009900100514        ENDSR   ;
010000100514       //-------------------------------------------------------
010100100514       //   Controlli da tabella 3EW
010200100514       //-------------------------------------------------------
010300100514       BEGSR  Tabella3EW    ;
010400100514
010500100514       ke1 ='0' + %editc(§3cCKS:'X')    ;
010600100514
010700100514       clear  SerieOK   ;
010800100514       clear  FlsOK   ;
010900100514
011000100514       setll  ('3EW': ke1) tntbe01l   ;
011100100514       reade  ('3EW': ke1) tntbe01l   ;
011200100514
011300100514 1     dow not %eof(tntbe01l)  ;
011400100514 2     if tbeatb=' '   ;
011500100514
011600100514       D3EW=tbeuni    ;
011700100514
011800120816 3     if §3ewNRS=inrsNRS  and  §3ewFAA=*blank;
011900100514         SerieOK='S'   ;
012000100514
012100100514       // Se trovata la serie controllo anche la filiale segnacollo
012200100514 4        if inrsFLS>0 and inrsFLS= §3ewFLS   ;
012300100514            FlsOK='S'   ;
012400100514 4        endif    ;
012500100514
012600100514       // Se passati segnacolli, li controllo
012700100514 4        if inrsfls=0 or (inrsFLS>0 and inrsFLS= §3ewFLS)  and
012800100514             inrsNCD>0     ;
012900100514           wparz_d=§3ewNSI   ;
013000100514           wparz_a=§3ewNSF   ;
013100100514
013200100514           exsr ContrSegnac ;
013300100514
013400100514 5         if no_range=' '   ;
013500100514           leave ;
013600100514 5         endif ;
013700100514 4        endif  ;
013800111216       // Passaggio info di serie parziale
013900111216          if §3ewNSI>1 or §3ewNSF<9999999;
014000111216             oNRSPRZ='P';
014100111216          endif;
014200100514
014300100514 3     endif  ;
014400100514 2     endif   ;
014500100514
014600100514       reade  ('3EW': ke1) tntbe01l   ;
014700100514 1     enddo   ;
014800100514
014900100514       // Se non ho trovato la serie corretta --> errore
015000100514 1     select    ;
015100100514 1     when   SerieOK<>'S'    ;
015200100514       onrsMSG= msg(4) ;
015300100514       onrsERR= 'S'   ;
015400100514
015500100514       // Se passata la filiale segnacollo e non congruente --> errore
015600100514 1     when  inrsFLS>0 and FlsOK<>'S'       ;
015700100514       onrsMSG= msg(5) ;
015800100514       onrsERR= 'L'   ;
015900100514
016000100514 x1    other    ;
016100100514
016200100514       // se passati i segnacolli e non trovati nel range errore
016300100514  2    if inrsNCD>0   and no_range='1'   ;
016400100514         onrsMSG= msg(6) ;
016500100514           %subst(onrsMSG:30:7)=%editc(wparz_d:'Z')  ;
016600100514           %subst(onrsMSG:38:7)=%editc(wparz_a:'Z')  ;
016700100514         onrsERR= 'N'   ;
016800100514  2    endif   ;
016900100514
017000100514  1    endsl   ;
017100100514
017200100514       ENDSR  ;
017300100514       //-------------------------------------------------------
017400100514       //   Controllo segnacolli DDAL  / AL
017500100514       //-------------------------------------------------------
017600100514       BEGSR  ContrSEGNAC   ;
017700100514
017800100514       clear       no_range  ;
017900100514
018000100514       if   inrsNCD<wparz_d or inrsNCD >wparz_a or inrsNCA >wparz_a  ;
018100100514        no_range='1'   ;
018200100514       endif   ;
018300100514
018400100514       ENDSR   ;
018500100514       //-------------------------------------------------------
018600100514       //   Controlli da tabella 3CP
018700100514       //-------------------------------------------------------
018800100514       BEGSR  Tabella3CP    ;
018900100514
019000100514       ke1 =%editc(inrsCCM:'X')    ;
019100100514
019200100514       clear  SerieOK   ;
019300100514       clear  wparz_d   ;
019400100514       clear  wparz_a   ;
019500100514       clear  w3cpNRS   ;
019600100514
019700100514       setll  ('3CP': ke1) tntbe01l   ;
019800100514       reade  ('3CP': ke1) tntbe01l   ;
019900100514
020000100514 1     dow not %eof(tntbe01l)  ;
020100100514 2     if tbeatb=' '   ;
020200100514
020300100514       D3CP=tbeuni    ;
020400100514       clear w007a  ;
020500100514       w007a  = %subst(tbeke2: 3: 7)    ;
020600100514
020700100514       wparz_d= %int(w007a)   ;
020800100514       wparz_a= §3cpAL        ;
020900100514       w3cpNRS= %int(%subst(tbeke2: 1: 2))  ;
021000100514       leave   ;
021100100514 2     endif   ;
021200100514
021300100514       reade  ('3CP': ke1) tntbe01l   ;
021400100514 1     enddo   ;
021500100514
021600100514       // Serie    ;
021700100514  1    select    ;
021800100514  1    when  w3cpNRS>0  ;
021900100514
022000100514  2    if    w3cpNRS<>inrsNRS   ;
022100100514         onrsMSG= msg(7) ;
022200100514         onrsERR= 'S'   ;
022300100514  x2   else   ;
022400100514
022500111216       // Memorizzo che si tratta di serie parziale
022600111216         oNRSPRZ='P';
022700100514       // controllo il range di segnacolli
022800100514  3    if inrsNCD>0   ;
022900100514           exsr ContrSegnac ;
023000100514  4       if  no_range='1'   ;
023100100514           onrsMSG= msg(9) ;
023200100514           %subst(onrsMSG:30:7)=%editc(wparz_d:'Z')  ;
023300100514           %subst(onrsMSG:38:7)=%editc(wparz_a:'Z')  ;
023400100514           onrsERR= 'N'   ;
023500100514  4       endif   ;
023600100514  3    endif   ;
023700111216
023800100514  2    endif   ;
023900100514
024000100517  1    when  §3cnrs<>inrsNRS
024100100517             or §3cokd<>'*'    ;
024200100517
024300100514         onrsMSG= msg(8) ;
024400100514         onrsERR= 'S'   ;
024500100517
024600100514       ENDSL  ;
024700100514
024800100514       ENDSR   ;
024900011011
025000060704**         MSG
025100100514Codice Cliente non immesso                                                    01
025200100514Numero Serie non immessa                                                      02
025300100514Non trovata tabella 3C per il codice cliente : telefonare in SEDE!            03
025400100514Serie non assegnata al cliente per  EASYWEB                                   04
025500100514La filiale segnacollo non corrisponde alla serie assegnata al cliente         05
025600110905Superato range di segnacolli xxxxxxx/xxxxxxx assegnato al cliente             06
025700100514Serie non assegnata al cliente per  EASYSPED                                  07
025800100514Serie non assegnata al cliente                                                08
025900110905Superato range di segnacolli xxxxxxx/xxxxxxx assegnato al cliente             09
