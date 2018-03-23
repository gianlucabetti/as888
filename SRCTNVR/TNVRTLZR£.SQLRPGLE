000001071018     h dftactgrp(*no) actgrp(*caller)
000002071018      *--Compilazione
000003071018     H BNDDIR('YU04')
002300071018     Ftntlz00f  uf   E             disk
002301071207     Ftntlz01l  if   E           k disk    rename(tntlz000:tntlz001)
002302071207     f                                     prefix($)
002303071119     Fqsysprt   O    F  132        PRINTER OFLIND(*INOF)
002304071018      *--Servizio-----------------------------
002305071018     D Eseguito        C                   '0'
002306071018     D Errore          C                   '1'
002307071018
002308071018      *--SubProcedure (YU045M01 - Prospetto)---------------
002309071018     D Yu045M01        PR
002310071018     D  M01SocOld
002311071018     D                                     LIKE(tlzcsf)
002312071018     D  M01KccOld
002313071018     D                                     like(capoconto)
002314071018     D  M01KscOld
002315071018     D                                     LIKE(sottoconto)
002316071018     D  M01SocNew
002317071018     D                                     LIKE(tlzcsf)
002318071018     D  M01KccNew
002319071018     D                                     like(capoconto)
002320071018     D  M01KscNew
002321071018     D                                     LIKE(sottoconto)
002322071018     D  M01Esito                      1
002323071018     D esito           s              1
002324071018     d capoconto       s              6    inz('000400')
002325071018     d sottoconto      s              8    inz('00000000')
002326071018     d tlzcsfnew       s                   LIKE(tlzcsf)
002327071018     d tlzpdrnew       s                   LIKE(sottoconto)
003600951009?     *--------------------------------------------------------------*
003700951009?     *  DS                                                          *
003800951009?     *--------------------------------------------------------------*
003900951009     D KPJBA         E DS
003901071018     D tntlzds       E DS                  extname(tntlz00f)
003902071018     d rrntlz          s              9  0
003903071012     c*
003904071018     C     *ENTRY        PLIST
003905071018     C                   PARM                    KPJBA
003906071119     c                   seton                                        of
003907071207     c     ktlz          klist
003908071207     c                   kfld                    tlztip
003909071207     c                   kfld                    $tlzpdr
003910071207     c                   kfld                    tlzcsfnew
007601031119     C/EXEC SQL
007602071018     C+ DECLARE A1 CURSOR FOR SELECT rrn(tntlz00f), socsocnew FROM
007603071018     C+ tntlz00f join yusoc00f on tlzcsf = socsocold WHERE tlzcsf <> ' '
007604071210     C+ and tlzpdr <> ' ' and TLZTIP <>'T'
007607071012     C/END-EXEC
007608071012     C/EXEC SQL
007609071012     C+ OPEN A1
007610071012     C/END-EXEC
009300020405
009400020405     C                   DOU       SqlCod <> 0
010100030827     C/EXEC SQL
010200071018     C+ FETCH NEXT FROM A1 INTO :rrntlz, :tlzcsfnew
010300030827     C/END-EXEC
010500020405     c*
010600020405     C                   SELECT
010700020405     C                   WHEN      SqlCod = 0
010701071018     c     rrntlz        chain     tntlz00f
010704071018     c                   if        %found(tntlz00f)
010706071018     c                   clear                   tlzpdrnew
010707071018     c                   clear                   esito
010708071018     c                   move      tlzpdr        sottoconto
010709071018      *Nuovo Conto
010710071018     C                   CALLP     YU045M01(tlzcsf
010711071018     C                                     :capoconto
010712071018     C                                     :sottoconto
010713071018     C                                     :tlzcsfnew
010714071018     C                                     :capoconto
010715071018     C                                     :tlzpdrnew
010716071018     C                                     :esito)
010717071207      * stampo
010718071018     C                   IF        esito = Errore
010719071119     c                   if        *inof
010720071119     c                   except    testa
010721071119     c                   setoff                                       of
010722071119     c                   end
010723071119     c                   except    riga
010724071018     C                   iter
010725071018     C                   ENDIF
010726071207     c* annullo vecchio codice
010727071207     c                   eval      tlzatb = '*'
010728071207     C                   UPDATE    tntlz000
010729071207     c* controllo se esiste già altrimenti segnalo errore
010730071207     c                   move      tlzpdrnew     $tlzpdr
010731071207     c     ktlz          chain     tntlz01l
010732071207     c                   if        not %found(tntlz01l)
010733071207     c                   if        *inof
010734071207     c                   except    testa
010735071207     c                   setoff                                       of
010736071207     c                   end
010737071207     c                   except    riga1
010738071207     C                   ENDIF
010739071207     c                   end
011100020405     C                   WHEN      SqlCod = 100
011200020405     C                   WHEN      SqlCod <> 0
011300020417      * Forzo la stampa del JOBLOG.
011400020417     C                   CALL      'X66CHGJOB'
011500020405     C                   ENDSL
011600020404     C                   ENDDO
011700020417     C/EXEC SQL
011800020417     C+ CLOSE A1
011900020417     C/END-EXEC
012200951009     C*
012201071012     c                   seton                                        lr
012300071119     Oqsysprt   E            TESTA          1 02
012400071119     O                                            8 'YU1800R '
012500071119     O                                          +10 'ERRORE TRASCODIFICA TNTLZ'
012600071119     O                                           +0 '00F'
012700071119     O                                          +10 'Data:'
012800071119     O                       udate               +1 '  /  /  '
012900071119     O                                          120 'Pag.'
013000071119     O                       PAGE          Z     +1
013100071119     O          E            riga        1
013200071119     O                       tlzcsf              +0
013300071119     O                       sottoconto          +1
013400071119     O                                           +1 'soc. new:'
013500071119     O                       tlzcsfnew           +1
013600071207     O          E            riga1       1
013700071207     O                                           +0 ' Fornitore:'
014000071207     O                       tlzcsfnew           +1
014100071207     O                       tlzpdrnew           +1
014200071207     O                                           +1 ' Non trovato in archivio'
