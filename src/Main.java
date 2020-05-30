import java.io.File;
import java.io.FileWriter;
import org.antlr.v4.runtime.ParserRuleContext;
import org.snt.inmemantlr.GenericParser;
import org.snt.inmemantlr.listener.DefaultTreeListener;
import org.snt.inmemantlr.tree.ParseTree;
import org.snt.inmemantlr.utils.FileUtils;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonElement;
import com.google.gson.JsonParser;

public class Main {
	public static String jsonFormatter(String srcJsonString) {
		Gson gson = new GsonBuilder().setPrettyPrinting().create();
		JsonParser jp = new JsonParser();
		JsonElement je = jp.parse(srcJsonString);
		String prettyJsonString = gson.toJson(je);
		return prettyJsonString;
	}
	public static void main(String[] args) throws Exception {
		File file1 = new File("C.g4");
		if (!file1.exists()) {
			System.out.println("Can't find *.g4 file.");
		}
		GenericParser gp =  new GenericParser(file1);
		String inputFile = null;
		if (args.length > 0) inputFile = args[0];
		else System.out.println("input error");
		File file2 = new File(inputFile);
		if (!file2.exists()) {
			System.out.println("Can't find *.c file.");
		}
		String s = FileUtils.loadFileContent(file2);	
		DefaultTreeListener defaultTreeListener = new DefaultTreeListener();
		gp.setListener(defaultTreeListener);
		gp.compile();
		ParserRuleContext ctx = gp.parse(s, "compilationUnit", GenericParser.CaseSensitiveType.NONE);
		ParseTree parseTree = defaultTreeListener.getParseTree();
		
		String json = parseTree.toJson();
//		System.out.println(json);
		String prettyJsonString = jsonFormatter(json);
//		System.out.println(prettyJsonString);
		File outputFile = new File(file2.getName()+".json");
    	if (!outputFile.exists()) {
			outputFile.createNewFile();
		}
    	FileWriter fileWriter = new FileWriter(outputFile.getName());
    	fileWriter.write(prettyJsonString);
    	fileWriter.close();
	}	
}
