000100150521     H DFTACTGRP(*NO)
000200060906
000300060906      //********************************************************************************************
000400150521      //
000700060906      //********************************************************************************************
000800150521
000801150521     d tibsipec1i    e ds                  qualified inz(*EXTDFT)
000802150521     d tibsipec1o    e ds                  qualified inz(*EXTDFT)
000803150521     d tibsipec2i    e ds                  qualified inz(*EXTDFT)
000804150521
000806150521      /COPY GAITRASRC/SRCPROTOPR,TIBSIPEC0
000807150521
000810150521     D prmRqsOpCode...
000811150521     D                 s             10I 0
000812150521     D prmRpyOpCode...
000813150521     D                 s             10I 0
000814150521     D prmRqsFormat...
000815150521     D                 s             10A
000816150521     D prmRqsData...
000817150521     D                 s          32767A   varying
000818150521     D prmRqsDataSize...
000819150521     D                 s             10I 0
000820150521     D prmRpyFormat...
000821150521     D                 s             10A
000822150521     D prmRpyData...
000823150521     D                 s          32767A
000824150521     D prmRpyDataSize...
000825150521     D                 s             10I 0
000826150522     D
000827150522     D p_cdfisc        s             16
000828150522     D p_tipo          s              3
000829150522     D p_data          s               D
000830150603     D p_ksc           s              8
000831150603     D p_societa       s              3
000832150603     D p_snatura       s              1
000833150604
000834150604      /COPY QSYSINC/QRPGLESRC,QUSEC
000835150604     D lngTx01         S            256A
000836150604     D lngTx02         S             10I 0
000837150604     D                                     INZ(%SIZE(lngTx01))
000838150604     D lngTx03         S              7A
000839150604     D                                     INZ('BAR0019')
000840150604     D lngTx04         S             20A
000841150604     D                                     INZ('YBARMSG   *LIBL')
000842150604
000843150604     D quilngtx        PR                  EXTPGM('QUILNGTX')
000845150604     D  lngTx01                     256A   OPTIONS(*VARSIZE)                    Text string
000846150604     D  lngTx02                      10I 0                                      Length text string
000847150604     D  lngTx03                       7A                                        Message ID
000848150604     D  lngTx04                      20A                                        Qualified MSGF
000849150604     D  lngTx05                      16A   OPTIONS(*VARSIZE)                    Error code
000850150604
000851150521
000852150522     C     *entry        plist
000853150522     C                   parm                    p_cdfisc
000854150522     C                   parm                    p_tipo
000855150603     C                   parm                    p_ksc
000856150603     C                   parm                    p_societa
000857150603     C                   parm                    p_snatura
000858150522
012600060906      /FREE
012601150521
012602150521            *INLR = *ON;
012603150521
012604150603         if p_ksc = *blanks;
012605150522              tibsipec1i.cdfisc = p_cdfisc;
012606150522              tibsipec1i.tipopec = p_tipo;
012607150603
012701150521             prmRqsOpCode = 0;
012702150521             prmRpyOpCode = 0;
012703150521             prmRqsFormat = 'TIBSIPEC1I';
012704150521             prmRqsData = tibsipec1i;
012705150521             prmRqsDataSize = %size(TIBSIPEC1I);
012706150521             prmRpyFormat = 'TIBSIPEC1O';
012707150521             prmRpyData =  ' ';
012708150521             prmRpyDataSize = %size(TIBSIPEC1O);
012709150521
012710150603         else;
012717150603
012718150603              tibsipec2i.sottoconto = p_ksc;
012719150603              tibsipec2i.snatura = p_snatura;
012720150603              tibsipec2i.societa = p_societa;
012721150603              tibsipec2i.tipopec = p_tipo;
012722150603
012724150603             prmRqsOpCode = 0;
012725150603             prmRpyOpCode = 0;
012726150603             prmRqsFormat = 'TIBSIPEC2I';
012727150603             prmRqsData = tibsipec2i;
012728150603             prmRqsDataSize = %size(TIBSIPEC2I);
012729150603             prmRpyFormat = 'TIBSIPEC1O';
012730150603             prmRpyData =  ' ';
012731150603             prmRpyDataSize = %size(TIBSIPEC1O);
012732150603
012733150603         endif;
012734150603
012735150603         Get_Indirizzo_PEC(prmRqsOpCode:prmRpyOpCode:
012736150603               prmRqsFormat:prmRqsData:prmRqsDataSize:
012737150603               prmRpyFormat:prmRpyData:prmRpyDataSize);
012738150603
012739150603                 tibsipec1o = prmRpyData ;
012740150603
012741150604            lngTx01 = 'prmRpyOpCode=' + %char(prmRpyOpCode) +
012742150604                      ' indpec=' + %trim(tibsipec1o.indpec) +
012743150604                      ' tipopec=' + %trim(tibsipec1o.tipopec) +
012744150604                      ' dtinval=' + %char(tibsipec1o.dtinval) +
012745150604                      ' dtfival=' + %char(tibsipec1o.dtfival) ;
012746150605
012747150605            lngtx02 = %len(%trim(lngtx01));
012748150605            if prmRpyOpCode = *zero;
012749150605            lngtx03 = 'BAR0102';
012750150605            endif;
012751150604            exsr displayLongText;
012752150604
012800150521            return;
012801150604
012802150604
012804150605       BEGSR displayLongText;
012811150604
012812150604         CLEAR qusec;
012813150604         qusbprv = %SIZE(qusec);
012814150604         quilngtx(lngTx01:lngTx02:lngTx03:lngTx04:qusec);
012815150604
012816150604       ENDSR;
012817150604
014500060906
014600060906      /END-FREE
