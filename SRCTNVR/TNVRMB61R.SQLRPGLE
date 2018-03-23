000100171122      //---------------------------------------------------------------
000300171122      //?      Pulisce FIPDO e FIPRO rimasti
000500171122      //---------------------------------------------------------------
000501171122     hDECEDIT('0,') DATEDIT(*yMd.) option(*nodebugio)
000701171121
000900171122      //---------------------------------------------------------------
001000171122      //? Dichiarazione file.
001100171122      //---------------------------------------------------------------
001600120423
002900171122      //---------------------------------------------------------------
003000171122      //? Definizione strutture dati.
003100171122      //---------------------------------------------------------------
003101171122     dFIDST00F       e ds                  extname(FIDST00F)
003102171122     dFIOR90DS       e ds
003103171122     dkpjba          e ds
003105171122
004100120423
004200171122      //---------------------------------------------------------------
004300171122      // Definizione variabili globali.
004400171122      //---------------------------------------------------------------
004403171122     d wEoF            s               n   inz
004405171122     d wData           s              8s 0 inz
005800120423
005809171122      //----------------------------------------------------------------
005810171122      // Programmi/Prototipi
005811171122      //----------------------------------------------------------------
005812171122      // - Driver per reperire mail/sms
005813171122     d FIOR90R         pr                  extpgm('FIOR90R')
005816171122     d  kpjba                              likeds(kpjba)
005817171122     d  fior90ds                           likeds(fior90ds)
005818171122
005820171122
007500171122      //---------------------------------------------------------------
007600171122      // ?M A I N - L I N E
007700171122      //---------------------------------------------------------------
009800171122     c     *entry        plist
009801171122     c                   parm                    kpjba
009802171122
009900171122       exec sql set option  DynUsrPrf = *Owner, CloSqlCsr = *EndMod;
009901171121
010200171122      //?Imposto le date
010300171122       wData = %dec(%subst(kpjbu:1:8):8:0);
010301171121
011300171122       wEoF = *off;
011301171121
011500171122      //?Leggo tutte le distinte a PDA chiuse e con data inferiore alla data lancio
011600171122       exec sql DECLARE DST cursor FOR
011800171122       SELECT DSTfgs, DSTnfv from FIDST00F join FIPDO00T on
011801171122       DSTfgs = PDOfgs and DStnfv = PDOndc
012000171122       WHERE  DSTdfv < :wData and DSTatb = ' ' and DSTfcf = 'S' and
012100171122       DSTpda in('C', 'E', 'O')
012101171122       GROUP By DSTfgs, DSTnfv
012200171122       ORDER BY DSTfgs, DSTnfv;
012300130430
012400171122       exec sql open DST;
012600171122       IF  sqlcode < 0;
012700171122         wEoF = *on;
012800171122       ENDIF;
012900130304
013000171122       DOW not wEoF;
013100171122         exec sql FETCH NEXT from DST into :DSTfgs, :DSTnfv;
013200171121
013300171122         IF  sqlcod = 100 or sqlcod < 0;
013400171122           wEOF = *on;
013500171122           leave;
013600171122         ENDIF;
013601171122
013602171122       // Se non c'è la fase RQE leggo altra distinta
013603171122         clear fior90ds;
013604171122         OR90tla = 'C';
013605171122         OR90fas = 'RQE';
013606171122         OR90fgs = DSTfgs;
013607171122         OR90ndc = DSTnfv;
013608171122         FIOR90R (kpjba:FIOR90DS);
013609171122         IF  OR90err <> *blanks;
013610171122           iter;
013611171122         ENDIF;
013613171121
013614171122       // Pulisco FIPDO e FIPRO
013615171122         exec sql
013616171122         DELETE from FIPDO00T where PDOfgs = :DSTfgs and PDOndc = :DSTnfv;
013617171122         exec sql
013618171122         DELETE from FIPRO10F where PROfgs = :DSTfgs and PROndc = :DSTnfv;
013619171122
017600171122       ENDDO;
017700130304
017900171122       exec sql close DST;
018000171121
018900171122       *inLR = *on;
019000171122        return;
019100171122
