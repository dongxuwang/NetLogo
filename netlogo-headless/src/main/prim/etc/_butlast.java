// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.prim.etc;

import org.nlogo.core.I18N;
import org.nlogo.core.LogoList;
import org.nlogo.core.Pure;
import org.nlogo.core.Syntax;
import org.nlogo.nvm.ArgumentTypeException;
import org.nlogo.nvm.RuntimePrimitiveException;
import org.nlogo.nvm.Reporter;

public final class _butlast
    extends Reporter
    implements Pure {
  @Override
  public Object report(final org.nlogo.nvm.Context context) {
    return report_1(context, args[0].report(context));
  }

  public Object report_1(final org.nlogo.nvm.Context context, Object arg0) {
    if (arg0 instanceof LogoList) {
      LogoList list = (LogoList) arg0;
      if (list.isEmpty()) {
        throw new RuntimePrimitiveException(context, this,
            I18N.errorsJ().getN("org.nlogo.prim.etc.$common.emptyListInput", displayName()));
      }
      return list.butLast();
    } else if (arg0 instanceof String) {
      String string = (String) arg0;
      if (string.length() == 0) {
        throw new RuntimePrimitiveException(context, this,
            I18N.errorsJ().getN("org.nlogo.prim.etc.$common.emptyStringInput", token().text()));
      }
      return string.substring(0, string.length() - 1);
    } else {
      throw new ArgumentTypeException(context, this, 0, Syntax.ListType() |
          Syntax.StringType(), arg0);
    }
  }

  public String report_2(final org.nlogo.nvm.Context context, String arg0) {
    if (arg0.length() == 0) {
      throw new RuntimePrimitiveException(context, this,
          I18N.errorsJ().getN("org.nlogo.prim.etc.$common.emptyStringInput", token().text()));
    }
    return arg0.substring(0, arg0.length() - 1);
  }

  public LogoList report_3(final org.nlogo.nvm.Context context, LogoList arg0) {
    if (arg0.isEmpty()) {
      throw new RuntimePrimitiveException(context, this,
          I18N.errorsJ().getN("org.nlogo.prim.etc.$common.emptyListInput", displayName()));
    }
    return arg0.butLast();
  }
}
