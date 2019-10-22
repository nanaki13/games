package bon.jo.view;

import java.awt.Component;
import java.awt.Dimension;
import java.io.IOException;
import java.net.URL;
import java.util.HashSet;
import java.util.Iterator;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.JEditorPane;
import javax.swing.JFrame;
import javax.swing.JTextArea;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;
import javax.swing.text.html.HTMLEditorKit;

public class HTMLText extends JFrame{
	
	public void create(){
		JEditorPane txt = new EditorPaneLinkDetector();
		//txt.setEditorKit(new HTMLEditorKit());
		//String str = "www.yahoo.com";
		//txt.setText(str);
		txt.addHyperlinkListener(new HyperlinkListener() {
			public void hyperlinkUpdate(HyperlinkEvent e) {
				URL clickedURL = e.getURL();
				if (e.getEventType().equals(HyperlinkEvent.EventType.ACTIVATED))
					try {
						BrowserLauncher.openURL(clickedURL.toString());
					} catch (IOException e1) {
						e1.printStackTrace();
					}
			}

		});
		//txt.setWrapStyleWord(true);
		this.getContentPane().add(txt);
		txt.setEditable(true);
		this.setSize(new Dimension(200,150));
		this.setVisible(true);
		
		
	}

	public static void main(String[] ar){
		HTMLText htt = new HTMLText();
		htt.create();
		//htt.setSize(new Dimension(400,400));
		htt.setTitle("Hyper Link Detector");
	}
}
