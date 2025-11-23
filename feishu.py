import argparse
import requests
import json
from datetime import datetime
import csv

# ======================
# 1. 配置
# ======================
def load_config(path="config.json"):
    with open(path, "r", encoding="utf-8") as f:
        return json.load(f)

def get_tenant_access_token(app_id, app_secret):
    url = "https://open.feishu.cn/open-apis/auth/v3/tenant_access_token/internal/"
    resp = requests.post(url, json={"app_id": app_id, "app_secret": app_secret}).json()
    if resp.get("code") != 0:
        raise RuntimeError(f"获取token失败: {resp}")
    return resp["tenant_access_token"]


# ======================
# 2. 获取数据
# ======================
def fetch_records(app_token, bitable_app_token, table_id, limit=500):
    url = f"https://open.feishu.cn/open-apis/bitable/v1/apps/{bitable_app_token}/tables/{table_id}/records"
    headers = {"Authorization": f"Bearer {app_token}"}
    params = {"page_size": limit}
    records, page_token = [], None

    while True:
        if page_token:
            params["page_token"] = page_token
        resp = requests.get(url, headers=headers, params=params).json()
        if resp.get("code") != 0:
            raise RuntimeError(f"获取数据失败: {resp}")
        # 健壮性处理，确保 items 为列表
        items = resp.get("data", {}).get("items", [])
        if not items:
            break
        records.extend(items)
        page_token = resp["data"].get("page_token")
        if not page_token:
            break
    return records

def records_to_csv(records, filename):
    """
    将记录转换为CSV格式并保存到文件
    """
    if not records:
        # 如果没有记录，创建一个空的CSV文件
        with open(filename, 'w', newline='', encoding='utf-8') as f:
            writer = csv.writer(f)
            writer.writerow([])  # 创建一个空行
        return
    
    # 提取字段名作为列标题
    fieldnames = set()
    for record in records:
        if 'fields' in record:
            fieldnames.update(record['fields'].keys())
    
    fieldnames = sorted(list(fieldnames))
    
    # 写入CSV文件
    with open(filename, 'w', newline='', encoding='utf-8') as f:
        writer = csv.DictWriter(f, fieldnames=['record_id'] + fieldnames)
        writer.writeheader()
        
        for record in records:
            row = {'record_id': record.get('record_id', '')}
            fields = record.get('fields', {})
            row.update(fields)
            writer.writerow(row)

# ======================
# 3. 主流程
# ======================
def etl_pipeline(cfg):

    start_time = datetime.now()
    token = get_tenant_access_token(cfg["app_id"], cfg["app_secret"])
    print("===== [ETL START] =====")

    # 初始化结果信息
    result = {
        "success": False,
        "duration": 0,
        "start_time": start_time.strftime("%Y-%m-%d %H:%M:%S"),
        "message": "",
        "details": ""
    }

    try:
        # 抓取源表
        source_records = fetch_records(token, cfg["bitable_app_token"], cfg["source_table"])
        print(f"[INFO] 抓取源表 {len(source_records)} 条")

        # 如果配置了CSV输出，则将记录保存为CSV文件
        if cfg.get("csv_output", False):
            csv_filename = cfg.get("csv_file_name", "output.csv")
            records_to_csv(source_records, csv_filename)
            print(f"[INFO] 已将记录保存至 {csv_filename}")

    except Exception as e:
        result["message"] = str(e)
        print(f"[ERROR] ETL流程执行失败: {e}")
        raise
    finally:
        # 计算执行时间
        end_time = datetime.now()
        result["duration"] = (end_time - start_time).total_seconds()

    print("===== [ETL DONE] =====")

if __name__ == "__main__":
    # 添加命令行参数解析
    parser = argparse.ArgumentParser(description="飞书多维表格ETL工具")
    parser.add_argument("-c", "--config", default="config.json", help="配置文件路径")

    args = parser.parse_args()
    
    # 加载配置
    cfg = load_config(args.config)

    etl_pipeline(cfg)